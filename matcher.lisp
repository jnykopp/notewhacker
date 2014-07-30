;;;; Copyright 2013 Janne Nykopp

;;;; matcher.lisp

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

;;; For matching midi-events and target notes.
;;; 
;;; When a note-on event is received, the corresponding key is
;;; registered in a pressed-and-held-keys list. Note-off event clears
;;; that key from the pressed-and-held-keys list.
;;; 
;;; Held keys should be only matched to targets when new note-on
;;; events come in, as this indicates the user has done something.
;;; 
;;; If a target is matched, the corresponding keys are also cleared
;;; from the pressed-and-held-keys list, so that the user must release the
;;; keys and hit them again in order to hit a similar chord or key.
;;; 
;;; TODO: Doesn't yet support multiple targets when having multiple
;;; staffs (e.g. G and F clef staffs).

(defparameter *pressed-and-held-keys* (list)
  "Current state of keys pressed.")

(defgeneric update-status-with (event current-status)
  (:documentation "Update the CURRENT-STATUS with EVENT."))

(defmethod update-status-with ((event note-on-midi-event) current-status)
  "Set the key matching EVENT on."
  (pushnew (get-key event) current-status))
(defmethod update-status-with ((event note-off-midi-event) current-status)
  "Set the key matching EVENT off."
  (remove (get-key event) current-status))

(defun update-pressed-and-held-keys (midi-events)
  "Update the current pressed-and-held-keys with events listed in
  MIDI-EVENTS."
  (dolist (event midi-events)
    (setf *pressed-and-held-keys* (update-status-with event *pressed-and-held-keys*))))

(defun clear-given-keypresses (mkn-list)
  "Clear the pressed-and-held pressed-and-held-keys of the keys matching the
  MKNs in given MKN-LIST"
  (setf *pressed-and-held-keys* (set-difference *pressed-and-held-keys* mkn-list)))

#+5am
(progn
  (def-suite matcher-tests :description "Test the matching facility.")
  (in-suite matcher-tests))

#+5am
(test toggle-notes-on-off
  "Toggle some notes on and off. See that the status is correct."
  (let ((ev1 (make-instance 'note-on-midi-event :velocity 15 :key 64 :channel 0))
        (ev2 (make-instance 'note-on-midi-event :velocity 25 :key 68 :channel 0))
        (ev3 (make-instance 'note-off-midi-event :velocity 0 :key 64 :channel 0))
        status)
    (setf status (update-status-with ev1 status))
    (setf status (update-status-with ev2 status))
    (setf status (update-status-with ev1 status))
    (is (= (length status) 2))
    (is (find 64 status))
    (is (find 68 status))
    (setf status (update-status-with ev3 status))
    (is (= (length status) 1))
    (is (find 68 status))
    (is (not (find 64 status)))))

(let (unhandled-octets)                 ; container for incomplete input
  (defun get-new-midi-events ()
    "Handle the midi octets. Return a list of events."
    (multiple-value-bind (events unhandled)
        (create-midi-events-from-octets (concatenate 'vector
                                                     unhandled-octets
                                                     (get-elements *midi-octet-buffer*)))
      (setf unhandled-octets unhandled)
      events)))

(defun handle-midi-events-and-notify (new-events)
  "Handle the list of new midi events NEW-EVENTS. If there are new
  triggering events (note-on events for now) which trigger other
  actions, they are returned in a list."
  (update-pressed-and-held-keys new-events)
  (remove-if-not (lambda (x) (eq (type-of x) 'note-on-midi-event)) new-events))

(defun target-hit-p (target-mkns)
  "Check if the pressed-and-held-keys contains the specified
  TARGET-MKNS (a list of midi key numbers representing one
  chord). Return nil if it doesn't."
  (null (set-exclusive-or target-mkns
                          (intersection target-mkns *pressed-and-held-keys*))))

(defun filter-targets-hit (target-list)
  "Check if the pressed-and-held keys contain any of the target chords
  in TARGET-LIST (a list of chords, each chord being a list of midi
  key numbers). Return the target chords that were matched."
  (remove-if-not #'target-hit-p target-list))

(defun return-miss-events (events target-list)
  "User pressed some keys, creating new key-on-midi-events which are
listed in EVENTS. There are target chords listed in TARGET-LIST (a
list of chords, each chord being a list of midi key numbers). Return
the events that didn't hit any target note of any target chord."
  (let ((all-target-mkns (reduce #'append target-list)))
    (remove-if (lambda (x) (find (get-key x) all-target-mkns))
               events)))
