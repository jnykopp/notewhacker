;;;; Copyright 2024 Janne Nykopp

;;;; oss-midi.lisp

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

(defparameter *midi-device-pathname* nil
  "If set, read raw midi octets from this file. If not set, dynamically
try to figure out the correct file.")

(defparameter *midi-reader-quit-signal*
  (bordeaux-threads:make-lock "midi-reader-quit-lock")
  "A lock which the main thread holds as long as it wants the midi
  reader thread to stay alive.")

#+notewhacker-midi-thru
(defparameter *midi-thru-pathname*
  #.(merge-pathnames "midi-thru"
                     (uiop:parse-unix-namestring (uiop:getenv "HOME") :type :directory))
  "What is the file name for writing 'MIDI thru' octets to. This
  should be a FIFO which some synthesizer reads to produce sound,
  e.g.")

#+notewhacker-midi-thru
(let (midi-thru-stream)
  (defun open-midi-thru ()
    "Open the stream for midi thru."
    (unless midi-thru-stream
      (setf midi-thru-stream (open *midi-thru-pathname*
                                   :direction :output
                                   :if-exists :overwrite
                                   :if-does-not-exist :error
                                   :element-type '(unsigned-byte 8)))))
  (defun close-midi-thru ()
    "Close the stream for midi thru. Actually, this doesn't really
close the FIFO, since the listener may go in a panic if the
'device'-file vanishes. At least fluidsynth will cause 100% CPU
utilization.

NOTE: I wasn't able to test this with CCL so not sure if this will
work (the file handle is opened in a new thread, and next time,
possibly used in another thread, which the CCL might not like)."
    (when midi-thru-stream
      ;; Send "all notes off" control change to make sure no keys stay
      ;; pressed. (Not sure if this works with all MIDI equipment or
      ;; if a note-off event should be sent to all channels and all
      ;; keys...)
      (write-sequence '(176 123 0) midi-thru-stream)
      (force-output midi-thru-stream)))
  (defun midi-pass-through (octet)
    "Route the OCTET to midi thru stream.

Note that the OCTET ought to be sent immediately, skipping all I/O
buffering. Henec the call to `force-output', which forces the OCTET to
the stream unbuffered; output doesn't need implementation-specific
tricks, unlike reading a byte immediately from input stream."
    (when midi-thru-stream
      (write-byte octet midi-thru-stream)
      (force-output midi-thru-stream))))

(defun guess-midi-device-pathname ()
  (or (first (uiop:directory-files "/dev/" "midi*"))
      (error "No OSS midi device files found")))

(defun start-midi-reader-thread (pathname)
  "Start a thread which reads the midi messages from device file pointed to by PATHNAME."
  (unless (uiop:file-exists-p pathname)
    (error "Midi file (~s) doesn't exist" pathname))
  (unless (bt:acquire-lock *midi-reader-quit-signal* nil)
    (error "Someone's holding *midi-reader-quit-signal*!"))
  (prog1
      (bt:make-thread
       (lambda () (raw-read-octets pathname *midi-octet-buffer*))
       :name "midi reader")
    (setf *curr-midi-driver* 'oss)))

(defun stop-midi-reader-thread ()
  "Stop the midi reader thread."
  (bt:release-lock *midi-reader-quit-signal*))

;;; Reading data from a midi device:
;;;
;;; Midi device reading thread uses blocking read in a busy-loop
;;; waiting for data from the device. Data is read into a ring
;;; buffer. Another thread can then read the data from the buffer and
;;; create midi events as necessary.
;;;
;;; For now, the device reading thread is only implemented for Clozure
;;; Common Lisp and SBCL. Reading the octet should be non-blocking and
;;; this requires implementation-specific code.
;;;
;;; The current implementation for both CCL and SBCL is naive and
;;; inefficient, reading only one byte at a time (even if there were
;;; more octets immediately available at the input device). But this
;;; seems to be quite enough for current needs. Also, it doesn't seem
;;; to be trivial to read more than one byte with non-blocking IO.

#+ccl
(defun %ccl-read-octet (stream buffer)
  "Read one octet to BUFFER."
  (declare (optimize (speed 3)))
  (let ((octet
         (handler-case
             (read-byte stream)
           (ccl:input-timeout nil))))
    (when octet
      #+notewhacker-midi-thru (midi-pass-through octet)
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
      #+notewhacker-midi-thru (midi-pass-through octet)
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
  #+notewhacker-midi-thru (open-midi-thru)
  #+ccl (%ccl-raw-read-octets midi-dev-path buffer)
  #+sbcl (%sbcl-raw-read-octets midi-dev-path buffer)
  #-(or ccl sbcl) (error "This lisp implementation is not supported.")
  #+notewhacker-midi-thru (close-midi-thru))
