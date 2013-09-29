;;;; Copyright 2013 Janne Nykopp

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

(defparameter *midi-device-pathname* #p"/dev/midi1"
  "Read raw midi octets from this file.")

(defparameter *raw-buffer-length* 256
  "How many octets the raw buffer will be long")

(defparameter *midi-reader-quit-signal*
  (bordeaux-threads:make-lock "midi-reader-quit-lock")
  "A lock which the main thread holds as long as it wants the midi
  reader thread to stay alive.")

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
  "The raw buffer which will be constantly filled with
  `*midi-reader-thread*' and read from the main program thread
  periodically.")

(defun start-midi-reader-thread ()
  "Start a thread which reads the midi device."
  (unless (bt:acquire-lock *midi-reader-quit-signal* nil)
    (error "Someone's holding *midi-reader-quit-signal*!"))
  (bt:make-thread
   (lambda () (raw-read-octets *midi-device-pathname* *midi-octet-buffer*))
   :name "midi reader"))

(defun stop-midi-reader-thread ()
  "Stop the midi reader thread."
  (bt:release-lock *midi-reader-quit-signal*))

(defun add-element (element buffer)
  "Add an ELEMENT to the BUFFER. In case of overflow, oldest elements
  are overwritten silently. Returns the buffer."
  (declare (optimize (speed 3))
           (type (unsigned-byte 8) element)
           (type raw-buffer buffer))
  (bt:with-lock-held ((raw-buffer-lock buffer))
    (with-slots (octet-array curr-start curr-end unread-size) buffer
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

(defun midi-status-octet-p (octet)
  "Check if OCTET is a midi status octet."
  (> octet #b1111111))

(defmacro with-default-midi-data-checking ((num-of-args raw-data ptr) &body body)
  "Wrap BODY in code that tests that midi data from RAW-DATA starting
  from index PTR seems to be in order (right number of argument octets
  etc.). If it's not, a suitable value will be returned."
  (let ((num-of-args-gs (gensym "num-of-args"))
        (raw-data-gs (gensym "raw-data"))
        (ptr-gs (gensym "ptr")))
    `(let ((,num-of-args-gs ,num-of-args)
           (,raw-data-gs ,raw-data)
           (,ptr-gs ,ptr))
       (if (< (- (length ,raw-data-gs) ,ptr-gs) (1+ ,num-of-args-gs))
           ;; Insufficient data. TODO: Log.
           (values nil ,ptr-gs)
           (let ((err-pos
                  (position-if 'midi-status-octet-p ,raw-data-gs :start (1+ ,ptr-gs)
                               :end (+ ,ptr-gs 1 ,num-of-args-gs))))
             (if err-pos
                 ;; Invalid data (status octet instead of data
                 ;; octet). TODO: Log.
                 (values nil err-pos)
                 ;; Data was ok.
                 (values ,@body (+ ,ptr-gs 1 ,num-of-args-gs))))))))

(defun gen-chan-handler-ignoring-n-args (num-of-args)
  "Creates a midi octet stream handler that just ignores status octet
  and NUM-OF-ARGS octets."
  (lambda (raw-data ptr)
    ;; (declare (type (simple-array (unsigned-byte 8)) raw-data)
    ;;          (type fixnum ptr))
    (with-default-midi-data-checking (num-of-args raw-data ptr)
      nil)))

(defun status-octet-channel-number (octet)
  "Get the channel number from a status octet OCTET."
  (declare (type (unsigned-byte 8) octet))
  (logand octet #b00001111))

(defun note-generic-handler (raw-data ptr note-on-p)
  "Generic handler for note midi message. Create the event from
RAW-DATA, offset by PTR. If NOTE-ON-P is non-nil and velocity is
greater than 0, the instance will be note-on. (Some midi devices send note-on
  with velocity of 0 instead of note-off.)

Return values consisting of the instance and number of octets
handled."
  (with-default-midi-data-checking (2 raw-data ptr)
    (let ((channel (status-octet-channel-number (aref raw-data ptr)))
          (key (aref raw-data (+ ptr 1)))
          (velocity (aref raw-data (+ ptr 2))))
      (declare (type (integer 0 127) key velocity)
               (type (integer 0 15) channel))
      (make-instance (if (and note-on-p (> velocity 0))
                         'note-on-midi-event
                         'note-off-midi-event)
                     :channel channel :key key :velocity velocity))))

(defun note-on-handler (raw-data ptr)
  "Create and return a note-on or note-off event. (Some midi devices
  send note-on with velocity of 0 instead of note-off.)"
  (note-generic-handler raw-data ptr t))

(defun note-off-handler (raw-data ptr)
  "Create and return a note-off event."
  (note-generic-handler raw-data ptr nil))

(defun sys-common-handler (raw-data ptr)
  "Handle a system common message. These come in various forms"
  (error "Sys-common-handler not yet implemented (data: ~a ~a)" raw-data ptr))

(defun sys-real-time-handler (raw-data ptr)
  "Handle a system real time message. Right now, they are ignored."
  (declare (ignore raw-data))
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

(defun octets-to-midi-event (raw-data ptr)
  "Create one midi-event from RAW-DATA. If the RAW-DATA is
  insufficient, return values nil and PTR. If RAW-DATA is invalid,
  return values nil and PTR + number of octets so far handled. If
  RAW-DATA defines an event, return an event instance and PTR + number
  of octets handled. The event instance returned may be nil, if the
  event was to be ignored."
  (let ((handler (get-handler-for (aref raw-data ptr))))
    (funcall handler raw-data ptr)))

(defun create-midi-events-from-octets (raw-data)
  "Create new midi events from raw octet data RAW-DATA (vector or
nil). Raw data might contain incomplete event in the end. Returns
multiple values: a list of events, ordered by sequence of arrival, and
a vector containing remaining unhandled handled octets in the case
there was an incomplete event at the end. Malformed data is ignored."
  (if (and raw-data (> (length raw-data) 0))
      (let* ((data-ptr
              (position-if 'midi-status-octet-p raw-data)) ; Fast-forward to status octet.
             (data-left (not (null data-ptr)))
             events)
        (loop :while (and data-left (< data-ptr (length raw-data)))
           :do (multiple-value-bind (event new-ptr)
                   (octets-to-midi-event raw-data data-ptr)
                 (when event
                   (push event events))
                 (if (= new-ptr data-ptr)
                     ;; The event remaining was incomplete.
                     (setf data-left nil)
                     ;; Again fast-forward to next status octet if such is
                     ;; found. Ignore anything else, since it must be
                     ;; rubbish. TODO: Log.
                     (setf data-ptr
                           (or (position-if 'midi-status-octet-p raw-data :start new-ptr)
                               new-ptr)))))
        (values (reverse events) (subseq raw-data data-ptr)))
      (values nil nil)))

#+5am
(progn
  (def-suite midi-tests :description "Test the midi functionality.")
  (in-suite midi-tests))

#+5am
(test note-on-test
  "Try handling of note-on, followed by incomplete input for
note-off. Should produce note-on event and remaining data of 2
octets."
  (multiple-value-bind (events remaining-data)
      (create-midi-events-from-octets
       #(#b10010000 #b01000101 #b01100101 #b10000000 #b01000101))
    (is (= (length events) 1))
    (is (= (length remaining-data) 2))
    (is (= (aref remaining-data 0) #b10000000))
    (is (= (aref remaining-data 1) #b01000101))
    (let ((e (car events)))
      (is (eq (type-of e) 'note-on-midi-event))
      (is (= (get-channel e) 0))
      (is (= (get-key e) 69))
      (is (= (get-velocity e) 101)))))

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
      (is (= (get-velocity e2) 121)))))

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
      (is (= (get-velocity e) 0)))))

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

;;; Reading data from a midi device:
;;; 
;;; Midi device reading thread can block and wait for data. Busy-loop
;;; over the device, reading data into buffer. Another thread can then
;;; analyse data in the buffer and create midi events as necessary.
;;; 
;;; For now, the octet reader is only implemented for Clozure Common
;;; Lisp and SBCL. Reading the octet should be non-blocking and this
;;; requires implementation-specific code.
;;; 
;;; The current implementation for both CCL and SBCL is naive and
;;; inefficient, reading only one byte at a time. But this seems to be
;;; quite enough for current needs. Also, it doesn't seem to be
;;; trivial to read more than one byte with non-blocking IO.

#+ccl
(defun %ccl-read-octet (stream buffer)
  "Read one octet to BUFFER."
  (declare (optimize (speed 3)))
  (let ((octet
         (handler-case
             (read-byte stream)
           (ccl:input-timeout nil))))
    (when octet
      (add-element octet buffer))))

#+ccl
(defun %ccl-raw-read-octets (midi-dev-path buffer)
  (declare (optimize (speed 3)))
  (with-open-file (midi-dev midi-dev-path
                            :direction :input
                            :element-type '(unsigned-byte 8))
    (setf (ccl:stream-input-timeout midi-dev) 1)
    (loop :until (bt:acquire-lock *midi-reader-quit-signal* nil)
       :do (%ccl-read-octet midi-dev buffer))
    (bt:release-lock *midi-reader-quit-signal*)))

#+sbcl
(defun %sbcl-read-octet (stream buffer)
  "Read one octet to BUFFER."
  (declare (optimize (speed 3)))
  (let ((octet
         (handler-case
             (read-byte stream)
           (sb-sys:io-timeout nil))))
    (when octet
      (add-element octet buffer))))

#+sbcl
(defun %sbcl-raw-read-octets (midi-dev-path buffer)
  (declare (optimize (speed 3)))
  (with-open-file (midi-dev midi-dev-path
                            :direction :input
                            :element-type '(unsigned-byte 8))
    (let ((midi-dev-fd (sb-sys:make-fd-stream
                        (sb-sys:fd-stream-fd midi-dev)
                        :element-type '(unsigned-byte 8)
                        :buffering :none
                        :timeout 1)))
      (loop :until (bt:acquire-lock *midi-reader-quit-signal* nil)
         :do (%sbcl-read-octet midi-dev-fd buffer))
      (bt:release-lock *midi-reader-quit-signal*))))

(defun raw-read-octets (midi-dev-path buffer)
  "Blocking call (until the calling thread is signalled to terminate
  with *MIDI-READER-QUIT-SIGNAL*), which reads octets from opened
  device file OPEN-DEVICE to BUFFER."
  #+ccl (%ccl-raw-read-octets midi-dev-path buffer)
  #+sbcl (%sbcl-raw-read-octets midi-dev-path buffer)
  #-(or ccl sbcl) (error "This lisp implementation is not supported."))

