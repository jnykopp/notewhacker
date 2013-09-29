;;;; Copyright 2013 Janne Nykopp

;;;; notation.lisp

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

;;; This part handles creating typesetting music by their midi key
;;; numbers. X-coordinates in this system are system are multiples of
;;; notehead-width and Y-coordinates are multiples of
;;; notehead-heights. Coordinate system's origo is (X=irrelevant;
;;; Y=lowest staff line) .

;;; Terminology:
;;; MKN = midi key number
;;; MKNm = midi key number modulo 12 = midi key number within a octave

(defparameter *key-signature-name-to-num-of-sharp-or-flat*
  #.(let ((h (make-hash-table :test #'equalp)))
      (loop :for (key . value) :in
           '(("C Major" . (♯ 0))  ("A Minor" . (♯ 0))
             ("G Major" . (♯ 1))  ("E Minor" . (♯ 1))
             ("D Major" . (♯ 2))  ("B Minor" . (♯ 2))
             ("A Major"  . (♯ 3)) ("F♯ Minor" . (♯ 3))
             ("E Major"  . (♯ 4)) ("C♯ Minor" . (♯ 4))
             ("B Major"  . (♯ 5)) ("G♯ Minor" . (♯ 5))
             ("F♯ Major" . (♯ 6)) ("D♯ Minor" . (♯ 6))
             ("C♯ Major" . (♯ 7)) ("G♯ Minor" . (♯ 7))
             ("F Major" . (♭ 1)) ("D Minor" . (♭ 1))
             ("B♭ Major" . (♭ 2)) ("G Minor"  . (♭ 2))
             ("E♭ Major" . (♭ 3)) ("C Minor"  . (♭ 3))
             ("A♭ Major" . (♭ 4)) ("F Minor"  . (♭ 4))
             ("D♭ Major" . (♭ 5)) ("B♭ Minor" . (♭ 5))
             ("G♭ Major" . (♭ 6)) ("E♭ Minor" . (♭ 6))
             ("C♭ Major" . (♭ 7)) ("A♭ Minor" . (♭ 7)))
         :do (setf (gethash key h) value))
      h)
  "Mapping from key signature to accidental type and number of
  accidentals.")

(defun create-basic-mknm-pos-map ()
  "Map from MKNm to one or two choices of notehead position N (how
  many notehead heights up from (this octave's) C) and
  accidental. This is valid for key signature C Major or A Minor."
  (make-array '12
              :initial-contents
              '(((0 nil))
                ((0 ♯) (1/2 ♭))
                ((1/2 nil))
                ((1/2 ♯) (1 ♭))
                ((1 nil))
                ((3/2 nil))
                ((3/2 ♯) (2 ♭))
                ((2 nil))
                ((2 ♯) (5/2 ♭))
                ((5/2 nil))
                ((5/2 ♯) (3 ♭))
                ((3 nil)))))

(defparameter *sharp-key-signature-modifiers*
  #.(make-array 7
                :initial-contents
                '((5  ((3/2 ♮)) 6 ((3/2 nil)))
                  (0  ((0 ♮)) 1 ((0 nil)))
                  (7  ((2 ♮)) 8 ((2 nil)))
                  (2  ((1/2 ♮)) 1 ((1/2 nil)))
                  (9  ((5/2 ♮)) 10 ((5/2 nil)))
                  (4  ((1 ♮)) 5 ((1 nil)))
                  (11 ((3 ♮)) 0 ((-1/2 nil)))))
  "How the key signature with sharps modifies the note
  drawing. Mapping from the number of sharp signs minus one (first
  index on the list) to the two MKNm:s it modifies and their new
  mknm-draw-map entry (see CREATE-BASIC-MKNM-DRAW-MAP).")

(defparameter *flat-key-signature-modifiers*
  #.(make-array 7
                :initial-contents
                '((10 ((3 nil)) 11 ((3 ♮)))
                  (3  ((1 nil)) 4 ((1 ♮)))
                  (8  ((5/2 nil)) 9 ((5/2 ♮)))
                  (1  ((1/2 nil)) 2 ((1/2 ♮)))
                  (6  ((2 nil)) 7 ((2 ♮)))
                  (11 ((7/2 nil)) 0 ((0 ♮)))
                  (4  ((3/2 nil)) 5 ((3/2 ♮)))))
  "Like *SHARP-KEY-SIGNATURE-MODIFIERS* but for flats.")

(defun create-mknm-pos-map (key-signature)
  "Create a mknm map modified with KEY-SIGNATURE."
  (destructuring-bind (accidental number-of-accidentals)
      (gethash key-signature *key-signature-name-to-num-of-sharp-or-flat*)
    (let ((mkn-map (create-basic-mknm-pos-map))
          (mod-tbl (if (eq accidental '♯)
                   *sharp-key-signature-modifiers*
                   *flat-key-signature-modifiers*)))
      (dotimes (i number-of-accidentals mkn-map)
        (loop :for (indx modlst) :on (aref mod-tbl i) :by #'cddr
           :do (setf (aref mkn-map indx) modlst))))))

(defun mkn-to-octave-mknm (mkn)
  "Convert a MIDI key number MKN to octave and MKNm.
Return values octave, mknm."
  (multiple-value-bind (oct-plus-1 mknm) (floor mkn 12)
    (values (1- oct-plus-1) mknm)))

(defparameter *mknm-to-letter-name-accidental*
  '#((c . nil) (c . ♯) (d . nil) (d . ♯) (e . nil) (f . nil) (f . ♯)
     (g . nil) (g . ♯) (a . nil) (a . ♯) (b . nil))
  "Map a MKNm to letter name and accidental (on C-Major
  scale). Intended for use with `mkn-to-scientific-notation' and
  `scientific-notation-to-mkn'.")

(defun mkn-to-scientific-notation (mkn)
  "Convert a MIDI key number to scientific notation plus
accidental. Return values note-name, accidental, octave. Note: when
MKN could be written in multiple ways, only outputs the choice with
sharp or nil as accidental."
  (multiple-value-bind (oct mknm) (mkn-to-octave-mknm mkn)
    (destructuring-bind (name . accidental)
        (aref *mknm-to-letter-name-accidental* mknm)
      (values name accidental oct))))

(defun scientific-notation-to-mkn (name accidental octave)
  "Convert the scientific notation (NAME, ACCIDENTAL and OCTAVE) to
  midi key number. Return it. Note: Won't work if ACCIDENTAL is flat
  or neutral."
  (let ((mknm (position (cons name accidental)
                        *mknm-to-letter-name-accidental*
                        :test #'equalp)))
    (+ mknm (* (1+ octave) 12))))

(defun clef-and-c4-pos-on-clef (clef)
  "What is the position of the clef and C4 in notehead-heights from
  the lowest staff line of a staff with clef CLEF. Returned as
  multiple-values."
  (ccase clef 
    (g-clef (values 1 -1))
    (f-clef (values 3 5))))

(defclass notational-chord ()
  ((mkn-list
    :accessor mkn-list :initarg :mkn-list
    :documentation "List of all Midi Key Numbers bundled together in
    this note."))
  (:documentation "A chord's notational representation."))

(defclass notational-staff ()
  ((key-signature
    :reader key-signature :writer (setf key-signature) :initarg :key-signature
    :initform (error "Key signature must be set.")
    :documentation "Key signature on the staff. Should be some of
    strings in the *KEY-SIGNATURE-NAME-TO-NUM-OF-SHARP-OR-FLAT* hash
    map.")
   (clef
    :reader clef :writer (setf clef) :initarg :clef
    :initform (error "Clef must be set.")
    :documentation "Clef of the staff. Should be one of symbols known
    to the function `clef-and-c4-pos-on-clef'")
   (mknm-map
    :reader get-mknm-map
    :documentation "A map from MKNm to note position and
    accent. Depends on key-signature. Modified automatically when
    key-signature is changed.")
   (c4-pos
    :reader get-c4-pos
    :documentation "Position of note C4 on this staff as note-height
    steps from lowest staff line. Depends on clef. Modified
    automatically when clef is changed.")
   (clef-pos
    :reader get-clef-pos
    :documentation "Position of the middle-point of clef on this staff
    as noteheight steps from lowest staff line. Depends on
    clef. Modified automatically when clef is changed."))
  (:documentation "Notation-related data of a staff. With this
  information it should be possible to insert different notes etc. to
  the staff."))

(defgeneric (setf key-signature) (new-key inst)
  (:documentation "Set the key signature."))

(defmethod (setf key-signature) :after (new-key (inst notational-staff))
  "Handle additional tasks when setting NEW-KEY as the new key
signature of the staff INST."
  (setf (slot-value inst 'mknm-map) (create-mknm-pos-map new-key)))

(defgeneric (setf clef) (new-clef inst)
  (:documentation "Set clef."))

(defmethod (setf clef) :after (new-clef (inst notational-staff))
  "Handle additional tasks when setting NEW-CLEF as the new clef of
the staff INST."
  (declare (optimize (debug 3)))
  (multiple-value-bind (clef-pos c4-pos) (clef-and-c4-pos-on-clef new-clef)
    (setf (slot-value inst 'clef-pos) clef-pos)
    (setf (slot-value inst 'c4-pos) c4-pos)))

(defmethod initialize-instance :after ((staff notational-staff) &key)
  (setf (slot-value staff 'mknm-map) (create-mknm-pos-map (key-signature staff)))
  (multiple-value-bind (clef-pos c4-pos) (clef-and-c4-pos-on-clef (clef staff))
    (setf (slot-value staff 'clef-pos) clef-pos)
    (setf (slot-value staff 'c4-pos) c4-pos)))

(defun position-and-accidental-of-mkn (staff mkn choose-upper-position)
  "Determine the position (multiples of notehead-heights from lowest
  staff line) and accidental (symbol ♯, ♭ or nil) of given MKN on a
  STAFF (instance of notational-staff, with certain clef and key
  signature). If MKN defines a note that might be written in two
  different ways in the current key signature: If
  CHOOSE-UPPER-POSITION is non-nil, choose the position which is upper
  but has a flat accidental ♭. Otherwise, choose the position that is
  lower but has a sharp accidental ♯. E.g., When MKN is 68, it can be
  written in C-major scale as either G♯4 or A♭4."
  (multiple-value-bind (octave mknm) (mkn-to-octave-mknm mkn)
    (destructuring-bind (subpos accidental)
        (first (sort (aref (get-mknm-map staff) mknm)
                     (if choose-upper-position #'max #'min)
                     :key #'car))
      ;; Each octave differing from C4 lowers or raises by 3½
      ;; noteheads. MKNm modifies by [-½---3½].
      (values (+ (* (- octave 4) 7/2) subpos (get-c4-pos staff))
              accidental))))

(defun ledger-lines (position)
  "Determine the placement of ledger-lines for a note in
  POSITION (y-coordinate, as fractional multiples of notehead
  heights). Returns a list of y-coordinates of required ledger line
  midpoints (as fractional multiplies of notehead-heights).

  Note that these should always be natural numbers!"
  (let* ((flpos (floor position))
         (clpos (ceiling position)))
    (cond
      ;; Over the top of staff
      ((> flpos 4)
       (loop :for x :from 5 :upto flpos
          :collect x))
      ;; Under staff
      ((< clpos 0)
       (loop :for x :from -1 :downto clpos
          :collect x)))))

(defun ledger-lines-collection (positions)
  "Determine ledger lines for collection of notes (chord) whose
positions are listed in POSITIONS."
  (let (ledger-lines)
    (dolist (p positions ledger-lines)
      (setf ledger-lines (union ledger-lines (ledger-lines p))))))

(defun average (&rest numbers)
  "Calculate average of NUMBERS and return it. In case of no numbers,
returns addition operation's idempotent 0."
  (loop :for n :in numbers
     :for l from 1
     :summing n :into s :finally (return (/ s l))))

(defun draw-upside-down-p (positions)
  "Determine if the collection of notes (chord), whose positions are
  listed in POSITIONS as a list, should be drawn upside down. Returns
  non-nil if the note should be drawn upside down."
  (if (and positions (>= (apply 'average positions) 2))
      t
      nil))

;;; Language of graphics:
;;; Stack based.
;;; (save-transformation X) = save the transformation values, i.e.
;;; translate & rotate, and execute X
;;; (translate X Y) = move to (X, Y)
;;; (rotate X) = rotate by X degrees
;;; (notehead) = notehead with center in current point
;;; (♯) = sharp sign (♯) with center in current point
;;; (♭) = flat sign (♭) with center in current point
;;; (♮) = natural sign (♮) with center in current point
;;; (ledger-line L) = ledger-line of length L to current point, so
;;; that the center of the ledger line is in the current point.
;;; (stem H) = stem of height H with center point on current point

(defparameter *min-stem-height-nhh* 7/2
  "Stem height in notehead heights.")

(defparameter *stem-width-nhw* 1/7
  "Stem width in notehead widths.")

(defparameter *ledger-line-width-nhw* 3/2
  "Ledger line width in notehead widths.")

;;; TODO: This is huge function. Split.
(defun create-drawing-primitives-for-chord 
    (mkn-list staff &optional (choose-upper-position nil))
  "Generate a list of drawing primitives for drawing a chord
  consisting of (quarter)notes listed in MKN-LIST on STAFF. See
  CHOOSE-UPPER-POSITION description in
  `position-and-accidental-of-mkn'.

  Note that if MKN-LIST contains notes very close to each
  other (e.g. C4, C♯4, and D), it will result in omitting one of the
  noteheads (C4 specifically, which would overlap with one other
  anyway)."
  (let* ((pos-and-accidental
          (sort
           (mapcar (lambda (x)
                     (multiple-value-list
                      (position-and-accidental-of-mkn staff x choose-upper-position)))
                   mkn-list)
           #'> :key #'car))             ;sort positions from top to down
         (positions (mapcar #'car pos-and-accidental))
         (upside-down-p (draw-upside-down-p positions))
         (curr-pos (cons 0 0))
         (notehead-center-pos (+ -1/2 (/ *stem-width-nhw* 2)))
         earlier-note-upside-down-p     ;was previously drawn notehead upside-down? 
         crowded-note-positions         ;where the notes are crowded
         earlier-note-pos
         earlier-acc-pos
         collected-drawing-primitives)
    (flet ((move-to (x y)
             "Create a translate-call which will move to coordinate X,
             Y from the current position."
             (prog1 
                 `(translate ,(- x (car curr-pos)) ,(- y (cdr curr-pos)))
               (setf curr-pos (cons x y))))
           (invert (&rest x)
             "Create a call which creates code for inverting X."
             `(save-transformation
               (rotate 180)
               ,@x))
           (note-too-close-p (pos)
             (and earlier-note-pos (< (- earlier-note-pos pos) 1)))
           (acc-too-close-p (pos)
             (and earlier-acc-pos (< (- earlier-acc-pos pos) 1)))
           (push-call (x)
             (push x collected-drawing-primitives)))
      ;; Noteheads and accidentals
      (loop :for (pos acc) :in pos-and-accidental
         ;; Noteheads
         :do (let ((invp
                     (if (note-too-close-p pos)
                         (progn
                           (push pos crowded-note-positions)
                           (setf earlier-note-upside-down-p
                                 (not earlier-note-upside-down-p)))
                         upside-down-p)))
               (push-call (move-to (if invp
                                       (- notehead-center-pos)
                                       notehead-center-pos)
                                   pos))
               (push-call (if invp (invert '(notehead)) '(notehead)))
               (setf earlier-note-upside-down-p invp))
         ;; Accidentals TODO: Sometimes accidentals still get written
         ;; on top of each other.
         :when acc :do (progn
                         (push-call
                          (move-to (if (acc-too-close-p pos)
                                       ;; Should be shifted left
                                       -5/2
                                       -3/2)
                                   pos))
                         (push-call `(,acc)))
         :do (setf earlier-note-pos pos) (when acc (setf earlier-acc-pos pos)))
      ;; Stem
      (let* ((y-max (first positions))
             (y-min (car (last positions)))
             (stem-len (+ (- y-max y-min) *min-stem-height-nhh*))
             (center-y (if upside-down-p
                           (- y-max (/ stem-len 2))
                           (+ y-min (/ stem-len 2)))))
        (push-call (move-to 0 center-y))
        (push-call `(stem ,stem-len)))
      ;; Ledger lines
      (let ((max-crowded-note-pos (when crowded-note-positions
                                    (apply #'max crowded-note-positions)))
            (min-crowded-note-pos (when crowded-note-positions
                                    (apply #'min crowded-note-positions)))
            (base-x (if upside-down-p (- notehead-center-pos) notehead-center-pos)))
        (dolist (ll (ledger-lines-collection positions))
          (if (or (or (not max-crowded-note-pos) (> ll max-crowded-note-pos))
                  (or (not min-crowded-note-pos) (> ll min-crowded-note-pos)))
              (progn
                (push-call (move-to base-x ll))
                (push-call `(ledger-line ,*ledger-line-width-nhw*)))
              (progn
                (push-call (move-to 0 ll))
                (push-call `(ledger-line ,(* 3/2 *ledger-line-width-nhw*))))))))
    (reverse collected-drawing-primitives)))

(defun disentangle-drawing-primitives (drawing-primitives)
  "Dismantle DRAWING-PRIMITIVES into coordinate-primitive
parts. Return a list of elements (position list-of-drawing-commands)."
  (let ((curr-pos (cons 0 0))
        coords-and-elems)
    (dolist (primitive drawing-primitives coords-and-elems)
      (case (first primitive)
        (translate
         ;; In case of translate, update position.
         (setf curr-pos (cons (+ (car curr-pos) (second primitive))
                              (+ (cdr curr-pos) (third primitive)))))
        (save-transformation
         ;; Save-transformations can be removed.
         (push (list curr-pos (cdr primitive)) coords-and-elems))
        (t
         ;; Everything else is just appended to returned list.
         (push (list curr-pos (list primitive)) coords-and-elems))))))
