;;;; Copyright 2013 Janne Nykopp

;;;; notewhacker.lisp

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

(defparameter *debug* nil
  "Toggle on or off debugging features.")

(defun set-2d-projection (width height)
  "Sets the projection matrix for 2D-graphics for a window with
  dimensions (WIDTH x HEIGHT)."
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:viewport 0 0 width height)
  (gl:ortho 0 width 0 height 0 1)
  (gl:matrix-mode :modelview))

(defun get-target-points (target-chord-mkns)
  "Return the amount of points the target chord gives."
  (let ((num-of-notes (length target-chord-mkns)))
    (* 10 (expt 2 (1- num-of-notes)))))

(defun calculate-new-score (old-score points-awarded combo)
  "Calculate a score based on OLD-SCORE, POINTS-AWARDED for the
  targets hit and how long COMBO of correct notes player has. Return
  the new score."
  (+ old-score (* (1+ (/ combo 2)) points-awarded)))

(defstruct queue
  "Simple queue implementation. Tail always points to the last
element."
  (items nil :type list)
  (tail nil :type list))

(defun push-queue (q item)
  "Push the ITEM in queue Q. Return Q."
  (if (endp (queue-items q))
      (setf (queue-items q) (list item)
            (queue-tail q) (queue-items q))
      (setf (cdr (queue-tail q)) (list item)
            (queue-tail q) (cdr (queue-tail q))))
  q)

(defun peek-queue (q)
  "Return the first item in Q"
  (first (queue-items q)))

(defun pop-queue (q)
  "Pop the first item from Q"
  (prog1
      (pop (queue-items q))
    ;; Remove also the tail when queue is empty. (Tail is sometimes
    ;; used.)
    (when (queue-empty-p q)
      (setf (queue-tail q) nil))))

(defun queue-empty-p (q)
  "Check if Q is empty"
  (endp (queue-items q)))

(defclass game-staff (graphics-staff)
  ((target-chords
    :accessor target-chords :initarg :target-chords :initform (make-queue)
    :documentation "Targets scrolling on this staff. Queue of
    instances of graphics-chord. The current target is the first
    element (smallest x-coordinate). New elements go to back of the
    queue. Will be drawn as the last elements on the screen. Target
    notes advance at the speed of target-note-x-vel always.")
   (miss-notes
    :accessor miss-notes :initarg :miss-notes :initform nil
    :documentation "List of player's misses (hit the wrong note) on
    this staff. These also advance at the speed of
    target-note-x-vel.")
   (key-range
    :accessor key-range :initarg :key-range :initform nil
    :documentation "What range of keys (MKN) target this staff. Cons
    of min and max MKN or nil if no range specified. Used for matching
    Midi events to staffs.")
   (midi-channel
    :accessor midi-channel :initarg :midi-channel :initform nil
    :documentation "Events with matching channel information target
    this staff. If nil, all events match. Used for matching Midi
    events to staff."))
  (:documentation "Class holding information of a staff and its state
  in the game."))

(defgeneric target-chords-list (inst)
  (:documentation "Get the target-chords as a list."))
(defmethod target-chords-list ((inst graphics-staff))
  "List-like interface for the internal implementation of
  target-chords."
  (queue-items (target-chords inst)))

(defgeneric push-target-chord (inst item)
  (:documentation "Push a target chord at the end of the target chord
  list of INST"))
(defmethod push-target-chord ((inst graphics-staff) item)
  (push-queue (target-chords inst) item))

(defmethod draw :after ((inst game-staff))
  "Draw one staff. Relies on graphics-staff's draw, then just draws
the notes and target chords on top of it."
  (dolist (elem (append (target-chords-list inst) (miss-notes inst)))
    (draw elem)))

(defclass game-state ()
  ((staffs
    :accessor staffs :initarg :staffs
    :documentation "The staffs in the game. List of instances of
    game-staff.")
   (target-note-x-vel
    :accessor target-note-x-vel :initarg :target-note-x-vel :initform -0.5
    :documentation "Global X-velocity for all target notes. In target
    notes' case, their velocity is ignored and this is used instead.")
   (other-drawables
    :accessor other-drawables :initarg :other-drawables :initform nil
    :documentation "Other drawables. Will be drawn first so that they
    won't block the other drawable elements.")
   (score
    :accessor score :initarg :score :initform 0
    :documentation "Player's score.")
   (combo
    :accessor combo :initform 0
    :documentation "How many notes player has hit in a row without
    mistakes.")
   (num-of-misses
    :accessor num-of-misses :initform 0
    :documentation "How many misses the player has hit.")
   (num-of-hits
    :accessor num-of-hits :initform 0
    :documentation "How many hits the player has hit.")
   (lives
    :accessor lives :initform 5
    :documentation "How many lives the player has left."))
  (:documentation "State of a game."))

(defmethod draw ((inst game-state))
  "Draw the game-state."
  (dolist (elem (other-drawables inst))
    (draw elem))
  (dolist (staff (staffs inst))
    (draw staff))
  ;; Finally draw the scores etc. TODO: Hardcoded coordinates...
  (draw-string "Score:" 10 565)
  (draw-number (score inst) 150 565)
  (draw-string "Combo:" 10 530)
  (draw-number (combo inst) 150 530)
  (draw-string "Lives:" 550 565)
  (draw-number (lives inst) 660 565)
  (draw-string "Hits/misses:" 457 530)
  (let ((x 660))
    (incf x (draw-number (num-of-hits inst) x 530))
    (incf x (draw-string "/" x 530))
    (incf x (draw-number (num-of-misses inst) x 530))))

(defun current-target (staff)
  "Get the current target of the STAFF."
  (peek-queue (target-chords staff)))

(defun remove-current-target (staff)
  "Remove (and return) the current target from STAFF."
  (pop-queue (target-chords staff)))

(defun get-current-targets (game-state)
  "Return a list of the current target notes (instances of
graphics-chord). List will be at maximum as long as there are staffs
in the game."
  (mapcar #'current-target (staffs game-state)))

(defun %update-lifetimes (game-state)
  "Update the lifetimes of drawables and remove dead elements."
  (flet ((remove-dead (elem-list)
           (remove-if
            (lambda (x) (and (lifetime x) (< (lifetime x) 0)))
            elem-list)))
    (with-accessors ((od other-drawables)) game-state
      ;; Remove dead
      (setf od (remove-dead od))
      (let (misses-of-all-staffs)
        (dolist (staff (staffs game-state))
          (with-accessors ((mn miss-notes)) staff
            (setf mn (remove-dead mn))
            (setf misses-of-all-staffs (append misses-of-all-staffs mn))))
        ;; Update lifetimes
        (dolist (elem (append misses-of-all-staffs od))
          (when (lifetime elem)
            (decf (lifetime elem))))))))

(defun %update-positions (game-state)
  "Update game-state's element positions."
  (with-accessors ((od other-drawables) (tgt-x-vel target-note-x-vel)
                   (staffs staffs)) game-state
    ;; Target notes and missed notes travel at the target-note
    ;; velocity.
    (dolist (staff staffs)
      (dolist (elem (append (target-chords-list staff) (miss-notes staff)))
        (with-accessors ((pos pos)) elem
          (setf pos (cons (+ (car pos) tgt-x-vel) (cdr pos))))))
    ;; Other drawables obey their own velocity.
    (dolist (elem od)
      (with-accessors ((pos pos) (vel velocity)) elem
        (setf pos (cons-op #'+ pos vel))))))

(defun staff-matches-channel-p (staff channel)
  "Does a STAFF match the Midi event's CHANNEL."
  (let ((c (midi-channel staff)))
    (or (null c) (= channel c))))

(defun staff-matches-key-p (staff key)
  "Does a STAFF match the Midi event's KEY."
  (with-accessors ((kr key-range)) staff
    (if kr
        (destructuring-bind (min-k . max-k) kr
          (and (or (null min-k) (<= min-k key))
               (or (null max-k) (<= key max-k))))
        t)))

(defun select-staff-for-event (staffs event)
  "Return which staff from list of STAFFS the given midi-event EVENT
is for. Basically, the staff's midi-channel must match EVENT's midi
channel and staff's key-range must include EVENT's key.

If many staffs match, choose the one for which the event is closest to
any note of the target chord. If more than one staff's target are
equally close, choose the first matching staff in STAFFS list.

Might also return nil if no staffs match!"
  (with-accessors ((ev-chan get-channel) (ev-key get-key)) event
    (labels ((dist-to-target (tgt-chord)
               "Calculate the distance from EV-KEY to closest note in
                current target chord TGT-CHORD."
               (loop :for note :in (mkn-list tgt-chord)
                  :minimizing (abs (- note ev-key))))
             (dtt (staff)
               "Just a helper for using dist-to-target."
               (dist-to-target (current-target staff))))
      (let ((staffs-matching-event
             (remove-if-not
              (lambda (x) (and (staff-matches-channel-p x ev-chan)
                               (staff-matches-key-p x ev-key)))
              staffs)))
        (first
         (if (< (length staffs-matching-event) 2)
             staffs-matching-event
             (stable-sort staffs-matching-event
                          (lambda (x y) (< (dtt x) (dtt y))))))))))

(defun group-events-by-staff (staffs events)
  "Return a hash table with key being a staff in STAFFS and value
  being a list of events for that staff from list of EVENTS."
  (let ((hash (make-hash-table)))
    (dolist (ev events hash)
      (let ((s (select-staff-for-event staffs ev)))
        (when s
          (push ev (gethash s hash)))))))

(defun %create-miss-chord-for-staff (staff miss-events)
  "Create missed note markers for STAFF from MISS-EVENTS."
  (let ((miss-chord (create-chord (mapcar 'get-key miss-events) staff)))
    (setf (car (pos miss-chord))
          (car (pos (current-target staff))))
    (setf (color miss-chord) (list 1 0 0 1))
    (setf (lifetime miss-chord) 50)
    (setf (effects miss-chord)
          (lambda (inst)
            (color-fade-effect inst :a (lambda (x) (* x .9)))))
    miss-chord))

(defun %check-hits-misses (game-state events)
  "Check if the player scored some hits. Return a list of objects to
be appended to game-state's other drawable objects."
  ;; TODO: Split to two distinct functions.
  ;;
  ;; TODO: Matching ignores now midi channel and key range altogether!
  (declare (optimize (debug 3)))
  (when events
    (let* ((curr-tgts (get-current-targets game-state))
           (list-of-target-mkns
            (loop :for tgt in curr-tgts :when tgt :collect (mkn-list tgt)))
           (hit-tgt-chord-list (filter-targets-hit list-of-target-mkns))
           (miss-events (return-miss-events events list-of-target-mkns))
           pieces)
      (when miss-events
        ;; Game bookkeeping updates
        (setf (combo game-state) 0)
        (incf (num-of-misses game-state) (length miss-events))
        ;; Miss events don't belong directly to any staff (unless
        ;; staff has dedicated midi channel or key range). Pick a
        ;; staff that best matches the event. For each miss, create a
        ;; missed note marker for best matching staff.
        (loop
           :for staff :being :the :hash-keys :in
           (group-events-by-staff (staffs game-state) miss-events)
           :using (hash-value misses-for-staff)
           :do (pushnew
                (%create-miss-chord-for-staff staff misses-for-staff)
                (miss-notes staff))))
      (when hit-tgt-chord-list
        (with-accessors ((score score) (combo combo) (staffs staffs)) game-state
          ;; One midi event might match target of multiple staffs
          ;; (if staff ranges are overlapping). Thus, go through all
          ;; staffs with each hit chord.
          (dolist (staff staffs)
            (let* ((curr-tgt (current-target staff))
                   (curr-tgt-mkn-list (mkn-list curr-tgt)))
              (when (target-hit-p curr-tgt-mkn-list)
                (incf (num-of-hits game-state) (length curr-tgt-mkn-list))
                (setf score (calculate-new-score
                             score (get-target-points curr-tgt-mkn-list) combo))
                (incf combo)
                (detach-elem-from-paren curr-tgt)
                (remove-current-target staff)
                (let ((tmp-pieces (disassemble-graphics-element curr-tgt)))
                  ;; Make the pieces fly around and dwindle.
                  (dolist (piece tmp-pieces)
                    (setf (lifetime piece) 200)
                    (setf (velocity piece) (cons (/ (- (random 50) 25) 5)
                                                 (/ (- (random 50) 25) 5)))
                    (let ((rand-angle (/ (- (random 50) 25) 2)))
                      (setf (effects piece)
                            (let ((rot-angle rand-angle))
                              (lambda (inst)
                                (color-fade-effect inst
                                                   :g (lambda (x) (+ x .01))
                                                   :a (lambda (x) (* x .9)))
                                (gl:rotate rot-angle 0 0 1)
                                (incf rot-angle rand-angle)))))
                    (push piece pieces))))))))
      pieces)))

(defun %shake-staff (staff)
  "Make the STAFF shake a bit."
  (setf (effects staff)
        (let ((shake-for-this-many-frames 50))
          (lambda (inst)
            (decf shake-for-this-many-frames)
            (when (< shake-for-this-many-frames 0)
              (setf (effects inst) nil))
            (flet ((rand-shake ()
                     (* (random 10) (/ shake-for-this-many-frames 50.0))))
              (gl:translate (rand-shake) (rand-shake) 0))))))

(defun %create-target-out (fallen-chord)
  "Convert a FALLEN-CHORD attached to a staff to a spinning, fading
  chord not connected to any staff."
  (detach-elem-from-paren fallen-chord)
  (setf (velocity fallen-chord) (cons #1=(/ (- (random 20) 10) 5) #1#))
  (setf (lifetime fallen-chord) 200)
  ;; Effect: make the fallen chord spin around and fade out.
  (let ((rand-angle (/ (- (random 50) 25) 2)))
    (setf (effects fallen-chord)
          (let ((rot-angle rand-angle)
                (scl 1.1))
            (lambda (inst)
              (color-fade-effect inst
                                 :a (lambda (x) (* x .8))
                                 :r (lambda (x) (+ x .05)))
              (gl:rotate rot-angle 0 0 1)
              (gl:scale scl scl 1)
              (setf scl (* 1.1 scl))
              (incf rot-angle rand-angle)))))
  fallen-chord)

(defun %handle-targets-out (staff)
  "See if targets have gone off the staff. For those that did, remove
  these chords from staff's target list, detach them from staff, and
  return a list of these chords as drawable elements (with effects)
  which can be appended to other-drawables list of game state."
  (let ((tgt-queue (target-chords staff)))
    (let ((targets-out
           (loop
              :for target = (peek-queue tgt-queue)
              :while (and target (< (car (pos target)) 0))
              :collect (%create-target-out (pop-queue tgt-queue)))))
      (when targets-out
        (%shake-staff staff))
      targets-out)))

;;; TODO: User might need to modify this by hand! Implement a menu
;;; system for in-game settings.
(defun %create-new-target (staff score)
  "Create new targets to hit in staff STAFF, if it's time for
that. The SCORE may be used to generate targets of variable
difficulty. Return the new target."
  (let ((last-target (first (queue-tail (target-chords staff)))))
    ;; Create new targets when there are none, or the last target is
    ;; travelled far enough from the edge of the staff.
    (when (or (null last-target)
              ;; TODO: fixed coordinate
              (> (- (width staff) (car (pos last-target))) 150))
      ;; TODO: Better choice at random notes. Learning? (Make player try
      ;; and hit difficult notes, which player had problems with
      ;; earlier.)
      (let ((base-key (if (eq (clef staff) 'f-clef)
                          30
                          55
                          )))
        ;; NOTE: At the moment, this is tuned for the keyboard input.
        (flet ((random-note ()
                 (+ base-key (random 17))
                 ))
          (let ((note-num (1+ (random (ceiling (/ (1+ score) 500)))))
                notes)
            (dotimes (x note-num)
              (pushnew (random-note) notes))
            (when *debug*
              (format t "Creating chord ~a~&"
                      (mapcar (lambda (x)
                                (multiple-value-list (mkn-to-scientific-notation x)))
                              notes)))
            (create-chord notes staff)))))))

(defun game-state-step (game-state events)
  "Execute one step in the GAME-STATE, update element positions, check for
hits and misses etc according to EVENTS.

Return t, if game should still continue."
  (%update-lifetimes game-state)
  (%update-positions game-state)
  (with-accessors ((staffs staffs) (od other-drawables) (score score)
                   (tn-x-vel target-note-x-vel)) game-state
    (when events
      ;; TODO: This doesn't work yet for many staffs!
      (let ((pieces (%check-hits-misses game-state events)))
        ;; If target hit (new pieces appeared), speed up a bit
        (when pieces
          (setf od (append od pieces))
          (decf (target-note-x-vel game-state) 0.1))))
    (let (fallen-chords)
      (dolist (staff staffs)
        ;; Check if targets fell out.
        (setf fallen-chords (append fallen-chords (%handle-targets-out staff)))
        ;; Create new targets for each staff.
        (let ((new-tgt (%create-new-target staff score)))
          (when new-tgt
            (push-target-chord staff new-tgt))))
      ;; If there were fallen off chords, slow the game down, take one
      ;; life per fallen chord etc.
      (let ((fallen-chords-num (length fallen-chords)))
        (setf tn-x-vel (* (expt .5 fallen-chords-num) tn-x-vel))
        (decf (lives game-state) fallen-chords-num))
      ;; Append fallen chords to other drawables.
      (setf od (append od fallen-chords)))
    (> (lives game-state) 0)))

(defparameter *game-state* nil
  "Game state. A global variable for debugging purposes, for now.")

(defvar *highscore* 0
  "The highest score so far.")

(defun game-state-draw (win game-state)
  "Draw the GAME-STATE into GL context window WIN."
  (gl:clear-color 1 1 1 1)
  (gl:clear :color-buffer-bit)
  (draw game-state)
  (gl:flush)
  (sdl2:gl-swap-window win))

(defun gen-countdown (seconds)
  "Generate a countdown-state which will count down for specified
  time (SECONDS). Return a function which can be called with a GL
  context window and a list of new events."
  (let ((target-time (local-time:timestamp+ (local-time:now) seconds :sec)))
    (lambda (win new-events)
      (declare (ignore new-events))
      (gl:clear-color 1 1 1 1)
      (gl:clear :color-buffer-bit)
      (let ((secs-left
             (local-time:timestamp-difference target-time (local-time:now))))
        (if (< secs-left 0)
            (progn (setf target-time nil) (gen-game-loop))
            (progn (gl:color 1 1 1 1)
                   ;; TODO: Fixed coordinates
                   (gl:with-pushed-matrix
                     (gl:translate 395 300 0)
                     (draw-number (ceiling secs-left) 0 0))
                   (gl:flush)
                   (sdl2:gl-swap-window win)
                   nil))))))

(defun gen-game-loop ()
  "Create a new game loop state. Returns a function wich can be called
  with a GL context window and a list of new events."
  ;; TODO: *game-state* is global for debugging purposes (game can be
  ;; quit and the state still inspected afterwards). This could be a
  ;; closure for the returned function.
  ;;
  ;; TODO: Fixed coordinates!
  ;;
  ;; TODO: Each staff should have a target note generator function
  ;; (from user-defined configuration).
  (setf *game-state*
        (let ((g-staff (make-instance 'game-staff
                                      :width 700 :clef 'g-clef
                                      :key-signature "C Major"
                                      :midi-channel 0
                                      :pos (cons 50 350)))
              (f-staff (make-instance 'game-staff
                                      :width 700 :clef 'f-clef
                                      :key-signature "C Major"
                                      :midi-channel 1
                                      :pos (cons 50 125))))
          (make-instance 'game-state :staffs (list g-staff f-staff))))
  (lambda (win new-events)
    (let ((game-continues (game-state-step *game-state* new-events)))
      (game-state-draw win *game-state*)
      (unless game-continues
        (gen-cont-choice (score *game-state*))))))

(defun get-choice (new-events map-of-mkn-sym)
  "Examine the new midi-events NEW-EVENTS. If any of them has midi key
number which is mentioned in a map from mkn to a symbol
MAP-OF-MKN-SYM, return that matching symbol or nil if not found."
  ;; TODO: This is very rudimentary!
  (cdr (loop :for event :in new-events
          :when (find (get-key event) map-of-mkn-sym :test #'= :key #'car)
          :return it)))

(defun draw-choice (win score highscore)
  "Draw a screen to GL context window WIN which shows SCORE and
HIGHSCORE, and gives the choice to continue the game or quit."
  ;; TODO: This is very rudimentary!
  (gl:clear-color 1 1 1 1)
      (gl:clear :color-buffer-bit)
      ;; TODO: Fixed coordinates
      (gl:color 1 1 1 1)
      (draw-string "Highscore:" 120 400)
      (draw-string "Your score:" 120 360)
      (draw-number highscore 400 400)
      (draw-number score 400 360)
      (draw-string "C-4: new game" 150 250)
      (draw-string "G-4: quit game" 150 210)
      (gl:flush)
      (sdl2:gl-swap-window win))

(defun gen-cont-choice (score)
  "Create a choice-state for choosing whether to play a new game or
  not. Returns a function which can be called with a GL context window
  and a list of new events."
  (lambda (win new-events)
    ;; mkn 60 = C4, mkn 67 = G4
    (let ((choice (get-choice new-events '((60 . new-game) (67 . quit))))
          (old-highscore *highscore*))
      (when (> score *highscore*) (setf *highscore* score))
      (draw-choice win score old-highscore)
      (case choice
        (new-game
         (gen-countdown 3))
        (quit
         'quit) ;quit is a meta-state, will be recognized in main-loop
        (otherwise nil)))))

;;; TODO: This map really only works for finnish/swedish
;;; keyboard. Should be done using scancodes in keyboard handling
;;; event loop and injecting midi events somehow to matcher's
;;; `get-new-midi-events'. Current implementation is a quick and dirty
;;; hack. (Now even more of a hack, with cl-sdl2, as no more than one
;;; key press is processed at a time)
(let ((keypress-to-mkn-map
       ;; For list of keysym names, see SDL2's SDL_keycode.h and
       ;; replace "SDLK_" with ":scancode-".
       '(:scancode-nonusbackslash 55
         :scancode-a 56
         :scancode-z 57
         :scancode-s 58
         :scancode-x 59
         :scancode-c 60
         :scancode-f 61
         :scancode-v 62
         :scancode-g 63
         :scancode-b 64
         :scancode-n 65
         :scancode-j 66
         :scancode-m 67
         :scancode-k 68
         :scancode-comma 69
         :scancode-l 70
         :scancode-period 71
         :scancode-semicolon 72
         :scancode-slash 73)))
  (defun handle-kbd-event (pressed-p scancode mod-value)
    "For debugging purposes. Return a list of midi events matching
  pressed or lifted (determined by PRESSED-P) key corresponding to
  SCANCODE. See `handle-midi-events-and-notify' for reference."
    (loop :for (keysym mkn) :on keypress-to-mkn-map :by #'cddr
       :when (sdl2:scancode= scancode keysym)
       :collect (make-instance
                 (if pressed-p 'note-on-midi-event 'note-off-midi-event)
                 :channel (if (= (mod mod-value 2) 1) 1 0)
                 :key mkn :velocity 127))))

(let ((init-and-cleanup-functions '(graphics)))
  (defun initialize-notewhacker ()
    "Call functions listed in INIT-AND-CLEANUP-FUNCTIONS with INIT-
appended to them."
    (dolist (f init-and-cleanup-functions)
      (funcall (intern (concatenate 'string (symbol-name 'init-) (symbol-name f)) :notewhacker))))
  (defun cleanup-notewhacker ()
    "Call functions listed in INIT-AND-CLEANUP-FUNCTIONS in reverse
order with CLEANUP- appended to them."
    (dolist (f (reverse init-and-cleanup-functions))
      (funcall (intern (concatenate 'string (symbol-name 'cleanup-) (symbol-name f)) :notewhacker)))))

(defun main ()
  ;; TODO: Read config.
  ;; Part of this code is derived from tutorial made by 3b
  ;; (http://3bb.cc/tutorials/cl-opengl/getting-started.html)
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :flags '(:shown :opengl))
      (sdl2:with-gl-context (gl-context win)
        (sdl2:gl-make-current win gl-context)
        (set-2d-projection 800 600)
        (gl:clear-color 1 1 1 1)
        ;; TODO: If midi reader won't start, fall back to keyboard input.
        (start-midi-reader-thread)
        (initialize-notewhacker)

        (gl:enable :blend)
        (gl:blend-func :src-alpha :one-minus-src-alpha)
        (gl:enable :line-smooth)
        (gl:hint :line-smooth-hint :nicest)
        (gl:enable :point-smooth)
        (gl:hint :point-smooth-hint :nicest)
        (gl:enable :polygon-smooth)
        (gl:hint :polygon-smooth-hint :nicest)

        (setf *random-state* (make-random-state t))

        ;; TODO: Keyboard handling as a midi event generator is quite a
        ;; hack right now.
        (unwind-protect
             (let (;; TODO: Now starts from the countdown. Should have a
                   ;; game menu as the beginning point.
                   (current-state (gen-countdown 3))
                   extra-midi-events    ;This is for kbd debugging
                   (prev-frame-ticks 0))
               (sdl2:with-event-loop (:method :poll)
                 (:quit () t)
                 (:keydown (:keysym keysym)
                     (let ((scancode (sdl2:scancode-value keysym))
                           (mod-value (sdl2:mod-value keysym)))
                       (when (sdl2:scancode= keysym :scancode-escape)
                         (sdl2:push-quit-event))
                       (when (sdl2:scancode= keysym :scancode-f1)
                         (setf *debug* (not *debug*)))
                       (setf extra-midi-events (handle-kbd-event t scancode mod-value))
                       (when (and *debug* extra-midi-events)
                           (format t "new events: ~a~%" extra-midi-events))))
                 (:keyup (:keysym keysym)
                     (let ((scancode (sdl2:scancode-value keysym))
                           (mod-value (sdl2:mod-value keysym)))
                       ;; This case needed only for kbd debugging!
                       (setf extra-midi-events (handle-kbd-event nil scancode mod-value))
                       (when (and *debug* extra-midi-events)
                         (format t "new events: ~a~%" extra-midi-events))))
                 (:idle ()
                        ;; Update screen only at about 60 FPS (16
                        ;; milliseconds between frames) because that's
                        ;; what the `game-state-step' expects.
                        (when (> (- (sdl2:get-ticks) prev-frame-ticks) 15)
                          (setf prev-frame-ticks (sdl2:get-ticks))
                          (let* ((new-events
                                  ;; Concatenate extra-midi-events for kbd
                                  ;; debugging.
                                  (handle-midi-events-and-notify
                                   (prog1 (concatenate 'list extra-midi-events
                                                       (get-new-midi-events))
                                     (setf extra-midi-events nil))))
                                 (new-state (funcall current-state win new-events)))
                            (if (eq new-state 'quit) ;quit is a special state
                                (sdl2:push-quit-event)
                                (when new-state (setf current-state new-state))))))))
          (stop-midi-reader-thread)
          (cleanup-notewhacker)
          (clear-texture-entity-cache))))))
