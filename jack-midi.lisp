;;;; Copyright 2024 Janne Nykopp

;;;; jack-midi.lisp

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

(defparameter *jack-client* nil "Jack client instance")
(defparameter *midi-in-port* nil "Jack-port for midi in")
(defparameter *jack-shut-down* nil "Has Jack been shut down?")

(cffi:defcallback jack-shutdown :void ((arg :pointer))
  (declare (ignore arg))
  (setf *jack-shut-down* t))

(cffi:defcallback jack-midi-callback :int ((nframes jack-nframes-t) (arg :pointer))
  (declare (ignore arg))
  (let* ((midi-in-buf (jack-port-get-buffer *midi-in-port* nframes))
         (ev-num (jack-midi-get-event-count midi-in-buf)))
    (cffi:with-foreign-object (in-event 'jack-midi-event-t)
      (loop :for ev-ind :below ev-num
            :do (jack-midi-event-get in-event midi-in-buf ev-ind)
                (loop :for i :below (cffi:foreign-slot-value
                                     in-event 'jack-midi-event-t 'size)
                      :do (add-element (cffi:mem-aref (cffi:foreign-slot-value
                                                       in-event 'jack-midi-event-t 'buffer)
                                                      'jack-midi-data-t i)
                                       *midi-octet-buffer*)))))
  0)

(defun jack-lib-init ()
  "Initialize Jack library"
  (when (or (null *jack-client*) (cffi:null-pointer-p *jack-client*))
    (cffi:define-foreign-library libjack (t (:default "libjack")))
    (cffi:load-foreign-library 'libjack)
    (cffi:with-foreign-string (client-name "Notewhacker")
      (cffi:with-foreign-object (status 'jackstatus)
        (setf *jack-client* (jack-client-open client-name :jacknulloption status))
        (when (cffi:null-pointer-p *jack-client*)
          (error "Jack client open failed. Status ~a" status))
        (jack-set-process-callback *jack-client* (cffi:callback jack-midi-callback) (cffi:null-pointer))
        (jack-on-shutdown *jack-client* (cffi:callback jack-shutdown) (cffi:null-pointer))
        (cffi:with-foreign-strings ((mi "midi_in") (mt +jack-default-midi-type+))
          (setf *midi-in-port* (jack-port-register *jack-client* mi mt :jackportisinput 0)))
        (when (cffi:null-pointer-p *midi-in-port*)
          (error "Jack midi in port is null"))
        (when (/= 0 (jack-activate *jack-client*))
          (error "Jack activate broken"))
        (setf *jack-shut-down* nil
              *curr-midi-driver* 'jack)))))

(defun jack-lib-deinit ()
  "Deinitialize Jack library"
  (unless *jack-shut-down*
    (jack-client-close *jack-client*))
  (setf *jack-client* nil
        *curr-midi-driver* nil)
  (cffi:close-foreign-library 'libjack))
