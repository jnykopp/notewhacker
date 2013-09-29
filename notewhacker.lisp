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

(defclass game-state ()
  ((staffs
    :accessor staffs :initarg :staffs
    :documentation "The staffs in the game.")
   (target-chords
    :accessor target-chords :initarg :target-chords :initform nil
    :documentation "Target chords. Instances of
   graphics-chord. Ordered list, where the current target is the first
   element (smallest x-coordinate). New elements go to back of the
   list. Will be drawn as the last elements on the screen. Target
   notes advance at the speed of target-note-x-vel always.")
   (miss-notes
    :accessor miss-notes :initarg :miss-notes :initform nil
    :documentation "List of player's misses (hit the wrong
    note). These also advance at the speed of target-note-x-vel.")
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
  (dolist (staff (staffs inst))
    (draw staff))
  (dolist (elem (append (miss-notes inst) (other-drawables inst)
                        (target-chords inst)))
    (draw elem))
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

(defun get-current-target (game-state)
  "Return the current target note (instance of graphics-chord)."
  (first (target-chords game-state)))

(defun %update-lifetimes (game-state)
  "Update the lifetimes of drawables and remove dead elements."
  (flet ((remove-dead (elem-list)
           (remove-if
            (lambda (x) (and (lifetime x) (< (lifetime x) 0)))
            elem-list)))
    (with-accessors ((mn miss-notes) (od other-drawables)) game-state
      (setf mn (remove-dead mn) od (remove-dead od))
      (dolist (elem (append mn od))
        (when (lifetime elem)
          (decf (lifetime elem)))))))

(defun %update-positions (game-state)
  "Update game-state's element positions."
  (with-accessors
        ((tc target-chords) (mn miss-notes) (od other-drawables)) game-state
    ;; Target notes and missed notes travel at the target-note
    ;; velocity,
    (dolist (elem (append tc mn))
      (with-accessors ((pos pos)) elem
        (setf pos (cons (+ (car pos) (target-note-x-vel game-state))
                        (cdr pos)))))
    ;; Other drawables obey their own velocity.
    (dolist (elem od)
      (with-accessors ((pos pos) (vel velocity)) elem
        (setf pos (cons-op #'+ pos vel))))))

(defun %check-hits-misses (game-state events)
  "Check if the player scored some hits. Return a list of new miss-elements."
  ;; TODO: Split to two distinct functions.
  (when events
    ;; TODO: doesn't work for multiple staffs right now
    (let* ((list-of-target-mkns (list (mkn-list (get-current-target game-state))))
           (hitp (which-targets-hit list-of-target-mkns))
           (miss-mkns (return-miss-mkns events list-of-target-mkns))
           misses
           pieces)
      (when miss-mkns
        ;; Create missed note markers
        (let ((miss-chord (create-chord miss-mkns (first (staffs game-state)))))
          (setf (car (pos miss-chord))
                (car (pos (get-current-target game-state))))
          (setf (color miss-chord) (list 1 0 0 1))
          (setf (lifetime miss-chord) 200)
          (setf (effects miss-chord)
                (lambda (inst)
                  (color-fade-effect inst :a (lambda (x) (* x .9)))))
          (setf (combo game-state) 0)
          (incf (num-of-misses game-state))
          (pushnew miss-chord misses)))
      (when hitp
        (let ((hit-note (get-current-target game-state)))
          (incf (num-of-hits game-state) (length (mkn-list hit-note)))
          (with-accessors ((score score) (tc target-chords) (combo combo))
              game-state
            (setf score
                  (calculate-new-score
                   score (get-target-points (mkn-list hit-note)) combo))
            (incf combo)
            (detach-elem-from-paren hit-note)
            (setf tc (rest tc)))
          (let ((tmp-pieces (disassemble-graphics-element hit-note)))
            ;; Make them fly around and dwindle.
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
              (push piece pieces)))))
      (values misses pieces))))

(defun %handle-target-out (game-state)
  "Check if some target chords went out of the playing area (player
  didn't hit them in time). Remove the chords from game-state's target
  list, detach them from the staff and return them as a list of
  drawable elements which can be appended to other-drawables list of
  game-state."
  (with-accessors ((tc target-chords) (tn-x-vel target-note-x-vel)) game-state
    (let ((chords-out (remove-if (lambda (x) (> (car x) 0)) tc :key 'pos)))
      (when chords-out
        ;; Make the staff shake. TODO: Only works for one staff for now.
        (setf (effects (first (staffs game-state)))
              (let ((shake-for-this-many-frames 50))
                (lambda (inst)
                  (decf shake-for-this-many-frames)
                  (when (< shake-for-this-many-frames 0)
                    (setf (effects inst) nil))
                  (flet ((rand-shake ()
                           (* (random 10) (/ shake-for-this-many-frames 50.0))))
                    (gl:translate (rand-shake) (rand-shake) 0)))))
        ;; Slow the game down, take one life etc.
        (setf tn-x-vel (* .5 tn-x-vel))
        (decf (lives game-state))
        ;; Remove fallen out targets from target notes. Target notes
        ;; should be kept sorted.
        (setf tc (sort (set-difference tc chords-out) #'<
                       :key (lambda (x) (car (pos x))))))
      ;; Detach fallen notes from staff and handle them otherwise too.
      (dolist (fallen-chord chords-out chords-out)
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
                    (incf rot-angle rand-angle)))))))))

(defun %create-new-targets (game-state)
  "Create new targets to hit, if it's time for that. Return a list of
them."
  ;; TODO: only works with one staff at the moment.
  (let ((staff (first (staffs game-state)))
        (tc (target-chords game-state)))
    ;; Create new targets when there are none, or the last target is
    ;; travelled far enough from the edge of the staff.
    (when (or (null tc)
              ;; TODO: fixed coordinate
              (> (- (width staff) (car (pos (car (last tc))))) 150))
      ;; TODO: Better choice at random notes. Learning? (Make player try
      ;; and hit difficult notes, which player had problems with
      ;; earlier.)
      (flet ((random-note ()
               (+ 52 (random 30))))
        (let ((note-num (1+ (random (ceiling (/ (1+ (score game-state)) 500)))))
              notes)
          (dotimes (x note-num)
            (pushnew (random-note) notes))
          (when *debug*
            (format t "Creating chord ~a~&"
                    (mapcar (lambda (x)
                              (multiple-value-list (mkn-to-scientific-notation x)))
                            notes)))
          (list (create-chord notes staff)))))))

(defun game-state-step (game-state events)
  "Execute one step in the game-state, update element positions, check for
hits and misses etc. Return t, if game should still continue."
  (%update-lifetimes game-state)
  (%update-positions game-state)
  (with-accessors ((mn miss-notes) (tc target-chords) (od other-drawables)) game-state
    (when events
      (multiple-value-bind (misses pieces)
          (%check-hits-misses game-state events)
        (setf mn (append mn misses)
              od (append od pieces))
        ;; If target hit (new pieces appeared), speed up a bit...
        (when pieces (decf (target-note-x-vel game-state) 0.1))))
    (setf od (append od (%handle-target-out game-state)))
    (dolist (new-tgt (%create-new-targets game-state))
      (setf tc (nconc tc (list new-tgt))))
    (> (lives game-state) 0)))

(defparameter *game-state* nil
  "Game state. A global variable for debugging purposes, for now.")

(defvar *highscore* 0
  "The highest score so far.")

(defun game-state-draw (game-state)
  "Draw the game state."
  (gl:clear-color 1 1 1 1)
  (gl:clear :color-buffer-bit)
;  (gl:color 1 1 1 1)
  (draw game-state)
  (gl:flush)
  (sdl:update-display))

(defun gen-countdown (seconds)
  "Generate a countdown-state which will count down for specified
  time (SECONDS). Return a function which can be called with a list of
  new events."
  (let ((target-time (local-time:timestamp+ (local-time:now) seconds :sec)))
    (lambda (new-events)
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
                   (sdl:update-display)
                   nil))))))

(defun gen-game-loop ()
  "Create a new game loop state. Returns a function wich can be called
  with a list of new events."
  ;; TODO: *game-state* is global for debugging purposes (game can be
  ;; quit and the state still inspected afterwards). This could be a
  ;; closure for the returned function.
  (setf *game-state*
        (let ((staff (make-instance 'graphics-staff
                                    :width 600 :clef 'g-clef
                                    :key-signature "C Major"
                                    :pos (cons 50 300))))
          (make-instance 'game-state :staffs (list staff))))
  (lambda (new-events)
    (let ((game-continues (game-state-step *game-state* new-events)))
      (game-state-draw *game-state*)
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

(defun draw-choice (score highscore)
  "Draw a screen which shows SCORE and HIGHSCORE, and gives the choice
to continue the game or quit."
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
      (sdl:update-display))

(defun gen-cont-choice (score)
  "Create a choice-state for choosing whether to play a new game or
  not. Returns a function which can be called with a list of new
  events."
  (lambda (new-events)
    ;; mkn 60 = C4, mkn 67 = G4
    (let ((choice (get-choice new-events '((60 . new-game) (67 . quit))))
          (old-highscore *highscore*))
      (when (> score *highscore*) (setf *highscore* score))
      (draw-choice score old-highscore)
      (case choice
        (new-game
         (gen-countdown 3))
        (quit
         'quit) ;quit is a meta-state, will be recognized in main-loop
        (otherwise nil)))))

(defun main ()
  ;; TODO: Read config.
  ;; Part of this code is derived from tutorial made by 3b
  ;; (http://3bb.cc/tutorials/cl-opengl/getting-started.html)
  (sdl:with-init ()
    (sdl:window 800 600 :title-caption "Notewhacker" :double-buffer t :bpp 32
                :flags sdl:sdl-opengl :opengl-attributes '((:sdl-gl-doublebuffer 1)
                                                           (:sdl-gl-accelerated-visual 1)))
    (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)
    (set-2d-projection 800 600)
    (gl:clear-color 1 1 1 1)
    (setf (sdl:frame-rate) 60)
    ;; TODO: If midi reader won't start, fall back to keyboard input.
    (start-midi-reader-thread)

    (gl:enable :blend)
    (gl:blend-func :src-alpha :one-minus-src-alpha)
    (gl:enable :line-smooth)
    (gl:hint :line-smooth-hint :nicest)
    (gl:enable :point-smooth)
    (gl:hint :point-smooth-hint :nicest)
    (gl:enable :polygon-smooth)
    (gl:hint :polygon-smooth-hint :nicest)

    (setf *random-state* (make-random-state t))

    (unwind-protect 
         (let (;; TODO: Now starts from the countdown. Should have a
               ;; game menu as the beginning point.
               (current-state (gen-countdown 3)))
           (sdl:with-events ()
             (:quit-event () t)
             (:key-down-event ()
                              (when (sdl:key-pressed-p :sdl-key-escape)
                                (sdl:push-quit-event))
                              (when (sdl:key-pressed-p :sdl-key-f1)
                                (setf *debug* (not *debug*))))
             (:idle ()
                    (let* ((new-events
                            (handle-midi-events-and-notify
                             (get-new-midi-events)))
                           (new-state (funcall current-state new-events)))
                      (if (eq new-state 'quit) ;quit is a special state
                          (sdl:push-quit-event)
                          (when new-state (setf current-state new-state)))))))
      (stop-midi-reader-thread)
      (clear-texture-entity-cache))))
