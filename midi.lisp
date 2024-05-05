;;;; Copyright 2013-2024 Janne Nykopp

;;;; midi.lisp

;;;;    This file is part of Notewhacker.
;;;;
;;;;    Notewhacker is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;
;;;;    Notewhacker is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU General Public License
;;;;    along with Notewhacker.  If not, see <http://www.gnu.org/licenses/>.

(in-package #:notewhacker)

(defparameter *raw-buffer-length* 256
  "How many octets the raw buffer will be long")

(defstruct raw-buffer
  "Buffer for raw octets"
  (octet-array (make-array *raw-buffer-length*
                           :element-type '(unsigned-byte 8)))
  (curr-start 0)
  (curr-end 0)
  (unread-size 0)
  (lock (bordeaux-threads:make-lock "buffer lock")))

(defparameter *midi-octet-buffer*
  (make-raw-buffer)
  "The raw buffer which will be constantly filled either from
  `*midi-reader-thread*' (if OSS midi) or from JACK callback
  `jack-midi-callback' and read from the main program thread
  periodically.")

(defun add-element (element buffer)
  "Add an ELEMENT to the BUFFER. In case of overflow, oldest elements
  are overwritten silently. Returns the buffer."
  (declare (optimize (speed 3))
           (type (unsigned-byte 8) element)
           (type raw-buffer buffer))
  (bt:with-lock-held ((raw-buffer-lock buffer))
    (with-slots (octet-array curr-start curr-end unread-size) buffer
      (declare (type (array (unsigned-byte 8)) octet-array)
               (type fixnum curr-start)
               (type fixnum curr-end)
               (type fixnum unread-size))
      (setf (aref octet-array curr-end) element)
      (let ((last-ind (1- (length octet-array))))
        (if (>= curr-end last-ind)
            (setf curr-end 0)
            (incf curr-end))
        (if (> unread-size last-ind)
          ;; Overflow.
          (if (>= curr-start last-ind)
              (setf curr-start 0)
              (incf curr-start))
          (incf unread-size)))))
  buffer)

(defun get-elements (buffer)
  "Get all elements from BUFFER. Returns an array of elements or NIL
if there are no elements to read."
  (declare (optimize (speed 3))
           (type raw-buffer buffer))
  (bt:with-lock-held ((raw-buffer-lock buffer))
    (with-slots (octet-array curr-start curr-end unread-size) buffer
      (declare (type (array (unsigned-byte 8)) octet-array)
               (type fixnum curr-start)
               (type fixnum curr-end)
               (type fixnum unread-size))
      (when (> unread-size 0)
        (prog1
            (if (> curr-end curr-start)
                (subseq octet-array curr-start curr-end)
                (if (= curr-start 0)
                    ;; Implicitly curr-end also 0 then -> just copy the
                    ;; whole sequence.
                    (subseq octet-array 0)
                    ;; Otherwise piecewise copy.
                    (let* ((oa-len (length octet-array))
                           (arr (make-array unread-size :element-type '(unsigned-byte 8))))
                      (setf (subseq arr 0) (subseq octet-array curr-start oa-len))
                      (setf (subseq arr (- oa-len curr-start))
                            (subseq octet-array 0 curr-end))
                      arr)))
          (setf curr-start curr-end)
          (setf unread-size 0))))))

#+5am
(progn
  (def-suite raw-buffer-tests :description "Test the raw buffer functionality.")
  (in-suite raw-buffer-tests))

#+5am
(def-fixture with-buffer ()
  (let ((buffer (make-raw-buffer)))
    (&body)))

#+5am
(test nothing-to-read-from-buffer
  "When there is nothing to read from the buffer, it should return
nil."
  (with-fixture with-buffer ()
    (is (eq (get-elements buffer) nil))))

#+5am
(test add-one-elem-to-buffer
  "After adding one element, reading all should return only that
element."
  (with-fixture with-buffer ()
    (add-element 111 buffer)
    (let ((res (get-elements buffer)))
      (is (= (length res) 1))
      (is (= (aref res 0) 111)))))
#+5am
(test fill-and-read-back-the-buffer
  "Filling the buffer completely and reading it back."
  (with-fixture with-buffer ()
    (loop :repeat *raw-buffer-length* :for x :from 0
       :do (add-element (mod x 256) buffer))
    (let ((res (get-elements buffer)))
      (is (= *raw-buffer-length* (length res)))
      (loop :for x :across res
         :for y :from 0
         :do (is (= (mod y 256) x))))))

#+5am
(test test-ring-continuity-of-buffer
  "Test writing over the edge of the buffer. It should still give the
elements in correct order."
  (with-fixture with-buffer ()
    (loop :repeat (- *raw-buffer-length* 2) :for x :from 0
       :do (add-element (mod x 256) buffer))
    (get-elements buffer)
    (loop :repeat 4 :for x :from 111
       :do (add-element x buffer))
    (let ((res (get-elements buffer)))
      (is (= 4 (length res)))
      (loop :for x :across res
         :for y :from 111
         :do (is (= y x))))))

#+5am
(test buffer-overflow
  "When too much is written to buffer, the earliest unread elements
  should be silently overwritten."
  (with-fixture with-buffer ()
    (loop :repeat (+ *raw-buffer-length* 10) :for x :from 0
       :do (add-element (mod x 256) buffer))
    (let ((res (get-elements buffer)))
      (is (= *raw-buffer-length* (length res)))
      (loop :for x :across res
         :for y :from 10
         :do (is (= (mod y 256) x))))))

(defclass midi-event ()
  ((timestamp
    :accessor get-timestamp :initarg :timestamp :type local-time:timestamp
    :initform (local-time:now)
    :documentation "Creation time of the midi event"))
  (:documentation "Base class for every midi event. Timestamped."))

(defclass channel-midi-event (midi-event)
  ((channel
    :accessor get-channel :initarg :channel :type (integer 0 15)
    :documentation "Channel number for the midi event"))
  (:documentation "A midi event with channel information."))

(defclass note-midi-event (channel-midi-event)
  ((key
    :accessor get-key :initarg :key :type (integer 0 127)
    :documentation "Midi key number")
   (velocity
    :accessor get-velocity :initarg :velocity :type (integer 0 127)
    :documentation "Key velocity"))
  (:documentation "A midi note event"))

(defclass note-on-midi-event (note-midi-event)
  ()
  (:documentation "A midi note-on event"))

(defclass note-off-midi-event (note-midi-event)
  ()
  (:documentation "A midi note-off event"))

(declaim (type (or null (integer #x80 #xef)) *midi-running-status*))
(defparameter *midi-running-status* nil
  "Voice and Mode Messages set the receiver in Running Status mode, where
status octets are not necessary if the status doesn't change. This
will be set to nil if any non voice or mode message is received, or
any System Exclusive or Common Status message is received. Real-time
messages won't affect running status.")

(defmethod print-object ((object note-midi-event) stream)
  "Print note midi event"
  (print-unreadable-object (object stream :type t)
    (with-slots (key velocity channel) object
      (format stream "midi key: ~a velocity: ~a channel: ~a" key velocity channel))))

(defun midi-status-octet-p (octet)
  "Return OCTET if OCTET is a midi status octet, else nil."
  (and (> octet #b1111111) octet))

(defmacro with-default-midi-data-checking ((num-of-args raw-data ptr) &body body)
  "Wrap BODY in code that tests that midi data from RAW-DATA starting
  from index PTR seems to be valid (right number of argument octets
  etc.). If it's not, a suitable value will be returned."
  (let ((num-of-args-gs (gensym "num-of-args"))
        (raw-data-gs (gensym "raw-data"))
        (ptr-gs (gensym "ptr")))
    `(let ((,num-of-args-gs ,num-of-args)
           (,raw-data-gs ,raw-data)
           (,ptr-gs ,ptr))
       (if (< (- (length ,raw-data-gs) ,ptr-gs) ,num-of-args-gs)
           ;; Insufficient data. TODO: Log.
           (values nil ,ptr-gs)
           (let ((err-pos
                   (position-if 'midi-status-octet-p ,raw-data-gs
                                :start ,ptr-gs :end (+ ,ptr-gs ,num-of-args-gs))))
             (if err-pos
                 ;; Invalid data (status octet instead of data
                 ;; octet). TODO: Log.
                 (values nil err-pos)
                 ;; Data was ok.
                 (values ,@body (+ ,ptr-gs ,num-of-args-gs))))))))

(defun gen-chan-handler-ignoring-n-args (num-of-args)
  "Creates a midi octet stream handler that just ignores NUM-OF-ARGS
octets."
  (lambda (status raw-data ptr)
    (declare (type (simple-array (unsigned-byte 8)) raw-data)
             (type fixnum ptr))
    (setf *midi-running-status* status)
    (with-default-midi-data-checking (num-of-args raw-data ptr)
      nil)))

(defun status-octet-channel-number (octet)
  "Get the channel number from a status octet OCTET."
  (declare (type (unsigned-byte 8) octet))
  (logand octet #b00001111))

(defun note-generic-handler (status raw-data ptr note-on-p)
  "Generic handler for note midi message. Create the event from
STATUS (the status octet) and RAW-DATA, offset by PTR. RAW-DATA at PTR
should start the data octets for the note. If NOTE-ON-P is non-nil and
velocity is greater than 0, the instance will be note-on. (Some midi
devices send note-on with velocity of 0 instead of note-off.)

Return values consisting of the instance and number of octets
handled."
  (with-default-midi-data-checking (2 raw-data ptr)
    (let ((channel (status-octet-channel-number status))
          (key (aref raw-data ptr))
          (velocity (aref raw-data (1+ ptr))))
      (declare (type (integer 0 127) key velocity)
               (type (integer 0 15) channel))
      (make-instance (if (and note-on-p (> velocity 0))
                         'note-on-midi-event
                         'note-off-midi-event)
                     :channel channel :key key :velocity velocity))))

(defun note-on-handler (status raw-data ptr)
  "Create and return a note-on or note-off event. (Some midi devices
  send note-on with velocity of 0 instead of note-off.)"
  (setf *midi-running-status* status)
  (note-generic-handler status raw-data ptr t))

(defun note-off-handler (status raw-data ptr)
  "Create and return a note-off event."
  (setf *midi-running-status* status)
  (note-generic-handler status raw-data ptr nil))

(defun sys-common-handler (status raw-data ptr)
  "Handle a system common message. These come in various forms"
  (setf *midi-running-status* nil)
  (error "Sys-common-handler not yet implemented (status: ~a; data: ~a ~a)"
         status raw-data ptr))

(defun sys-real-time-handler (status raw-data ptr)
  "Handle a system real time message. Right now, they are ignored."
  (declare (ignore status raw-data))
  (values nil (1+ ptr)))

(defparameter *midi-channel-status-handler-mapper*
  `((#b1000 . note-off-handler)                      ; Note on event
    (#b1001 . note-on-handler)                       ; Note off event
    (#b1010 . ,(gen-chan-handler-ignoring-n-args 2))  ; Polyph. Key Pressure
    (#b1011 . ,(gen-chan-handler-ignoring-n-args 2))  ; Control Change
    (#b1100 . ,(gen-chan-handler-ignoring-n-args 1))  ; Program Change
    (#b1101 . ,(gen-chan-handler-ignoring-n-args 1))  ; Channel Pressure
    (#b1110 . ,(gen-chan-handler-ignoring-n-args 2))) ; Pitch Wheel Change
  "Map a status octete of a midi event with channel information by its
  most-significant-nibble to a handler function.")

(defparameter *midi-system-status-handler-mapper*
  '((#b11110 . sys-common-handler)
    (#b11111 . sys-real-time-handler))
  "Map a status octet of a system status midi event by its 5 MSBs to a
  handler function.")

(defun get-handler-for (status-octet)
  "Get a handler according to the STATUS-OCTET."
  (or (cdr (assoc (ash status-octet -4) *midi-channel-status-handler-mapper*))
      (cdr (assoc (ash status-octet -3) *midi-system-status-handler-mapper*))))

(defun octets-to-midi-event (status raw-data ptr)
  "Create one midi-event from STATUS octet and RAW-DATA. If the RAW-DATA
is insufficient, return values nil and PTR. If RAW-DATA is invalid,
return values nil and PTR + number of octets so far handled. If
RAW-DATA defines an event, return an event instance and PTR + number
of octets handled. The event instance returned may be nil, if the
event was to be ignored."
  (let ((handler (get-handler-for status)))
    (funcall handler status raw-data ptr)))

(defun create-midi-events-from-octets (raw-data)
  "Create new midi events from raw octet data RAW-DATA (vector or
nil). Raw data might contain incomplete event in the end. Returns
multiple values: a list of events ordered by arrival time, earliest
first, and a vector containing remaining unhandled handled octets in
the case there was an incomplete event at the end. Malformed data is
ignored."
  (let ((ptr 0) (raw-data-len (length raw-data))
        events)
    (loop :with status := nil
          :while (< ptr raw-data-len)
          :do
             ;; Seek to next status octet or continue running status
             (loop :for candidate-status := (midi-status-octet-p (aref raw-data ptr))
                   :for curr-status := (or candidate-status *midi-running-status*)
                   :when (or candidate-status (not curr-status)) :do (incf ptr)
                   :do (setf status curr-status)
                   :until curr-status)
             ;; Then create an event
             (multiple-value-bind (event new-ptr)
                 (octets-to-midi-event status raw-data ptr)
               (when event (push event events))
               (if (= new-ptr ptr)
                   ;; The last event was incomplete, nothing more to do.
                   (loop-finish)
                   ;; Otherwise continue.
                   (setf ptr new-ptr))))
    (values (reverse events) (subseq raw-data (min ptr raw-data-len)))))

#+5am
(progn
  (def-suite midi-tests :description "Test the midi functionality.")
  (in-suite midi-tests))

#+5am
(test ignore-rubbish-data
  "Should not blow up if there's data without status octet, but should
skip to next status."
  (let (*midi-running-status*)
    (multiple-value-bind (events remaining-data)
        (create-midi-events-from-octets
         #(#b01000101 #b01100101 #b10000000 #b01000101 #b01100101))
      (is (= (length events) 1))
      (is (= (length remaining-data) 0))
      (let ((e (car events)))
        (is (eq (type-of e) 'note-off-midi-event))
        (is (= (get-channel e) 0))
        (is (= (get-key e) 69))
        (is (= (get-velocity e) 101)))
      (is (= *midi-running-status* #b10000000)))))

#+5am
(test note-on-test
  "Try handling of note-on, followed by incomplete input for
note-off. Should produce note-on event and remaining data of 1
octet and running-value of note-off on channel 1."
  (multiple-value-bind (events remaining-data)
      (create-midi-events-from-octets
       #(#b10010000 #b01000101 #b01100101 #b10000000 #b01000101))
    (is (= (length events) 1))
    (is (= (length remaining-data) 1))
    (is (= (aref remaining-data 0) #b01000101))
    (let ((e (car events)))
      (is (eq (type-of e) 'note-on-midi-event))
      (is (= (get-channel e) 0))
      (is (= (get-key e) 69))
      (is (= (get-velocity e) 101)))
    (is (= *midi-running-status* #b10000000))))

#+5am
(test note-on-running-status-test
  "Try handling of note-on, followed by other note-on using running
status, following incomplete note-on. Should produce two note-on events and
remaining data of 1 octet."
  (multiple-value-bind (events remaining-data)
      (create-midi-events-from-octets
       #(#b10010001 #b01000101 #b01100101 #b01000110 #b01100100 #b01000111))
    (is (= (length events) 2))
    (is (= (length remaining-data) 1))
    (is (= (aref remaining-data 0) #b01000111))
    (let ((e (first events)))
      (is (eq (type-of e) 'note-on-midi-event))
      (is (= (get-channel e) 1))
      (is (= (get-key e) 69))
      (is (= (get-velocity e) 101)))
    (let ((e (second events)))
      (is (eq (type-of e) 'note-on-midi-event))
      (is (= (get-channel e) 1))
      (is (= (get-key e) 70))
      (is (= (get-velocity e) 100)))
    (is (= *midi-running-status* #b10010001))))

#+5am
(test note-on-continue-from-running-status-test
  "Try handling of note-on when previous run had processed the running
status, following incomplete note-on. Should produce two note-on
events and remaining data of 1 octet."
  (let ((*midi-running-status* #1=#b10010010))
    (multiple-value-bind (events remaining-data)
        (create-midi-events-from-octets
         #(#b01000101 #b01100101 #b01000110 #b01100100 #b01000111))
      (is (= (length events) 2))
      (is (= (length remaining-data) 1))
      (is (= (aref remaining-data 0) #b01000111))
      (let ((e (first events)))
        (is (eq (type-of e) 'note-on-midi-event))
        (is (= (get-channel e) 2))
        (is (= (get-key e) 69))
        (is (= (get-velocity e) 101)))
      (let ((e (second events)))
        (is (eq (type-of e) 'note-on-midi-event))
        (is (= (get-channel e) 2))
        (is (= (get-key e) 70))
        (is (= (get-velocity e) 100)))
      (is (= *midi-running-status* #1#)))))

#+5am
(test note-on-off-test
  "Test handling of note-on followed by note-off."
  (multiple-value-bind (events remaining-data)
      (create-midi-events-from-octets
       #(#x94 #x3D #x1A #x83 #x3D #x79))
    (is (= (length events) 2))
    (is (or (null remaining-data) (= (length remaining-data) 0)))
    (let ((e1 (car events))
          (e2 (cadr events)))
      (is (eq (type-of e1) 'note-on-midi-event))
      (is (= (get-channel e1) 4))
      (is (= (get-key e1) 61))
      (is (= (get-velocity e1) 26))
      (is (eq (type-of e2) 'note-off-midi-event))
      (is (= (get-channel e2) 3))
      (is (= (get-key e2) 61))
      (is (= (get-velocity e2) 121)))
    (is (= *midi-running-status* #x83))))

#+5am
(test note-off-by-on-event-and-active-sensing
  "Test that note-on event with velocity 0 results in note-off
event. Also see that active sensing octets are ignored (but data
eaten)."
  (multiple-value-bind (events remaining-data)
      (create-midi-events-from-octets
       ;; Three active sensing octets at end, should be ignored.
       #(#x98 #x70 #x00 #xFE #xFE #xFE))
    (is (= (length events) 1))
    (is (or (null remaining-data) (= (length remaining-data) 0)))
    (let ((e (car events)))
      (is (eq (type-of e) 'note-off-midi-event))
      (is (= (get-channel e) 8))
      (is (= (get-key e) 112))
      (is (= (get-velocity e) 0)))
    (is (= *midi-running-status* #x98))))

#+5am
(test note-on-then-off-by-on-event-and-active-sensing
  "Test that note-on event with velocity 0 with running status results in
note-off event. Also see that active sensing octets are ignored (but
data eaten)."
  (multiple-value-bind (events remaining-data)
      (create-midi-events-from-octets
       ;; Three active sensing octets at end, should be ignored.
       #(#x98 #x70 #x30 #x70 #x00 #xFE #xFE #xFE))
    (is (= (length events) 2))
    (is (or (null remaining-data) (= (length remaining-data) 0)))
    (let ((e (first events)))
      (is (eq (type-of e) 'note-on-midi-event))
      (is (= (get-channel e) 8))
      (is (= (get-key e) 112))
      (is (= (get-velocity e) 48)))
    (let ((e (second events)))
      (is (eq (type-of e) 'note-off-midi-event))
      (is (= (get-channel e) 8))
      (is (= (get-key e) 112))
      (is (= (get-velocity e) 0)))
    (is (= *midi-running-status* #x98))))

#+5am
(test no-data-handling
  "Shouldn't blow up when no data is given."
  (multiple-value-bind (events remaining-data)
      (create-midi-events-from-octets nil)
    (is (null events))
    (is (or (null remaining-data) (= (length remaining-data) 0))))
  (multiple-value-bind (events remaining-data)
      (create-midi-events-from-octets #())
    (is (null events))
    (is (or (null remaining-data) (= (length remaining-data) 0)))))
