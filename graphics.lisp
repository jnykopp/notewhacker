;;;; Copyright 2013 Janne Nykopp

;;;; graphics.lisp

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

(defparameter *notation-font-pathname*
  (merge-pathnames #p"feta.ttf" (asdf:system-source-directory :notewhacker))
  "Font to use for the musical symbols.")

(defparameter *notation-font-loader*
  (zpb-ttf:open-font-loader *notation-font-pathname*)
  "Font loader for musical symbols.")

(defparameter *text-font-pathname*
  (merge-pathnames #p"CrimsonText-Roman.ttf"
                   (asdf:system-source-directory :notewhacker))
  "Font to use for regular text.")

(defparameter *text-font-loader*
  (zpb-ttf:open-font-loader *text-font-pathname*)
  "Font loader for regular text.")

(defparameter *notation-font-size* 72
  "Font size for notation. Temporary, should be calculated from the window size?")

(defparameter *text-font-size* 38
  "Font size for text. Temporary, should be calculated from the window size?")

;; Bounding box handling
(defun bb-height (bb)
  "What is the height of bounding box BB. Works for vecto and
sdl bounding boxes."
  (abs (- (aref bb 3) (aref bb 1))))
(defun bb-width (bb)
  "What is the width of bounding box BB. Works for vecto and sdl
bounding boxes."
  (- (aref bb 2) (aref bb 0)))
(defun get-vecto-bb-of (char)
  "Get the vecto bounding box of a single character CHAR"
  (vecto:string-bounding-box (vector char) *notation-font-size* *notation-font-loader*))

(defparameter *glyph-id-charcode-tbl*
  '((notehead . #x0056)
    (c-clef   . #x1d121)
    (f-clef   . #x00c7)
    (g-clef   . #x00c9)
    (♯        . #x002e)
    (♭        . #x003a)
    (♮        . #x0036))
  "Mapping from a glyph id to the corresponding charcode in the
  notation font file.")

(defparameter *notehead-height* 
  (bb-height (get-vecto-bb-of (cdr (assoc 'notehead *glyph-id-charcode-tbl*))))
  "Height of one notehead with the current font size.")

(defparameter *notehead-width*
  (bb-width (get-vecto-bb-of (cdr (assoc 'notehead *glyph-id-charcode-tbl*))))
  "Width of the quarter note head (assumed to be the same as half note
  head too).")

(defparameter *staff-line-width*
  (/ *notehead-height* 8)
  "Width of a staff line.")

(defparameter *stem-width* 
  (/ *notehead-height* 7)
  "Width of the stem (for any note with a stem, e.g. the quarter
  note).")

(defparameter *stem-height* 
  (* *notehead-height* 3)
  "Height of the stem (for any note with a stem, e.g. the quarter
  note).")

(defun draw-horizontal-line (x1 x2 y width &optional (color))
  "Draw a horizontal line of WIDTH from (X1, Y) to (X2, Y). Color is a
4 element list of RGBA within range 0-1."
  (let ((half-width (/ width 2.0)))
    (gl:with-pushed-matrix
      (gl:disable :texture-2d)
      (when color (apply 'gl:color color))
      (gl:with-primitive :quads
        (gl:vertex x1 (- y half-width))
        (gl:vertex x2 (- y half-width))
        (gl:vertex x2 (+ y half-width))
        (gl:vertex x1 (+ y half-width))))))

(defun draw-vertical-line (x y1 y2 width &optional (color))
  "Draw a vertical line of WIDTH from (X, Y1) to (X, Y2). COLOR is a
4 element list of RGBA within range 0-1."
  (gl:with-pushed-matrix
    (gl:translate x y1 0)
    (gl:rotate 90 0 0 1)
    (draw-horizontal-line 0 (- y2 y1) 0 width color)))

(defun glyph-id-to-charcode (id)
  "Translate ID to a charcode."
  (cdr (assoc id *glyph-id-charcode-tbl*)))

;;; The macro below is taken from a tutorial made by 3b
;;; (http://3bb.cc/tutorials/cl-opengl/textures-part-4.html) and
;;; modified a bit.
(defmacro with-vecto-canvas-as-texture ((width height) &body body)
  (let ((texture (gensym "TEXTURE-")))
    `(vecto:with-canvas (:width ,width :height ,height)
       ;; run some vecto code
       ,@body
       ;; and load the result into a texture
       (let ((,texture (car (gl:gen-textures 1))))
         (gl:bind-texture :texture-2d ,texture)
         (gl:tex-parameter :texture-2d :texture-min-filter :linear)
         (gl:tex-parameter :texture-2d :generate-mipmap t)
         (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
         (gl:tex-parameter :texture-2d :texture-border-color '(1 1 1 0))
         (gl:tex-parameter :texture-2d :texture-wrap-s :clamp)
         (gl:tex-parameter :texture-2d :texture-wrap-t :clamp)
         (gl:tex-image-2d :texture-2d 0 :rgba ,width ,height
                          0 :rgba :unsigned-byte
                          (vecto::image-data vecto::*graphics-state*))
         ,texture))))

(defclass texture-entity ()
  ((height
    :accessor height :initarg :height :type fixnum
    :documentation "Height of the texture-entity, in pixels.")
   (width
    :accessor width :initarg :width :type fixnum
    :documentation "Width of the texture-entity, in pixels.")
   (color
    :accessor color :initarg :color
    :documentation "Current color of the texture-entity. List of 4
    values RGBA in range [0-1]. This color can be applied when drawing
    the texture-entity (see draw-texture-entity's parameter
    apply-color).")
   (texture
    :accessor texture :initarg :texture
    :documentation "Texture for this texture-entity.")
   (bounding-box
    :accessor bounding-box :initarg :bounding-box
    :documentation "Bounding box of the texture-entity."))
  (:documentation "A GL-texture-entity."))

(defun draw-texture-entity
    (inst x y &optional (x-center t) (apply-color nil))
  "Draw a texture-entity INST to coordinates X and Y (screen
  coordinates) so that INST's bounding box origo point is drawn in
  that coordinate, or if X-CENTER is non-nil, the texture-entity's
  real x-center is drawn on that point. If APPLY-COLOR is non-nil, the
  color of the texture-entity is applied before drawing."
  (gl:with-pushed-matrix
    (gl:enable :texture-2d)
    (when apply-color
      (apply 'gl:color (color inst)))
    (gl:bind-texture :texture-2d (texture inst))
    (let* ((bb (bounding-box inst))
           (mid-x (/ (+ (aref bb 2) (aref bb 0)) 2))
           (left (+ x (aref bb 0)))
           (top (+ y (aref bb 1)))
           (right (+ left (width inst)))
           (bottom (+ top (height inst))))
      (when x-center
        (gl:translate (- mid-x) 0 0))
      (gl:with-primitive :quads
        (gl:tex-coord 0 0)
        (gl:vertex left bottom)
        (gl:tex-coord 1 0)
        (gl:vertex right bottom)
        (gl:tex-coord 1 1)
        (gl:vertex right top)
        (gl:tex-coord 0 1)
        (gl:vertex left top))
      (when *debug*
        (gl:disable :texture-2d)
        (gl:line-width 2)
        (gl:with-primitive :lines
          (gl:color 1 0 0)
          (gl:vertex (- x 5) y)
          (gl:vertex (+ x 5) y)
          (gl:vertex x (- y 5))
          (gl:vertex x (+ y 5)))
        (gl:with-primitive :quads
          (gl:color 0 0 1 .2)
          (gl:vertex left bottom)
          (gl:vertex right bottom)
          (gl:vertex right top)
          (gl:vertex left top))))))

(defparameter *gl-texture-entity-cache* (make-hash-table :test #'eq)
  "Creating a texture-entity, especially with vecto, can be
time-consuming. Thus this cache, mapping from an id to a
texture-entity. Id can be anything, e.g. glyph symbol, number or a
string. Contains all the (already instantiated) texture-entities. When
window is resized, this will be emptied and new properly-sized
textures instantiated again upon use.")

(defun delete-textures-from-cache ()
  "Delete textures from the texture-entity cache."
  (let (textures)
    (maphash (lambda (glyph texture-entity)
               (declare (ignore glyph))
               (push (texture texture-entity) textures))
             *gl-texture-entity-cache*)
    (gl:delete-textures textures)))

(defun clear-texture-entity-cache ()
  "Clear the texture-entity cache."
  (delete-textures-from-cache)
  (setf *gl-texture-entity-cache* (make-hash-table :test #'eq)))

(defun create-texture-entity-from-string-with-vecto
    (string size font-loader &optional (color (list 0 0 0 1)))
  "Create a texture-entity of a STRING using vecto with FONT-LOADER (being an
vecto font-loader loader instance) with SIZE."
  (let* ((bb (vecto:string-bounding-box string size font-loader))
         (w (ceiling (bb-width bb)))
         (h (ceiling (bb-height bb))))
    (let ((texture
           (with-vecto-canvas-as-texture (w h)
             (vecto:with-graphics-state
               (vecto:set-rgba-fill 1 1 1 1)
               (vecto:set-font font-loader size)
               (vecto:draw-string (- (aref bb 0)) (- (aref bb 1)) string)
               (vecto:stroke)))))
      (make-instance
       'texture-entity
       :texture texture :color color :width w :height h :bounding-box bb))))

(defun create-texture-entity-for-notation-glyph
    (glyph &optional (color (list 0 0 0 1)))
  "Create a texture-entity for notational GLYPH."
  (let ((charcode (glyph-id-to-charcode glyph)))
    (when *debug* (format t "Creating texture-entity for ~a~&" glyph))
    (create-texture-entity-from-string-with-vecto (vector charcode) *notation-font-size*
                                          *notation-font-loader* color)))

(defun create-texture-entity-for-string (string &optional (color (list 0 0 0 1)))
  "Create a texture-entity for a string STRING."
  (when *debug* (format t "Creating texture-entity for ~a~&" string))
  (create-texture-entity-from-string-with-vecto string *text-font-size*
                                        *text-font-loader* color))

(defun create-texture-entity-for-integer (value &optional (color (list 0 0 0 1)))
  "Create a texture-entity for a single integer with VALUE."
  (create-texture-entity-for-string (format nil "~d" value) color))

(defun create-texture-entity-for (thing &optional (color (list 0 0 0 1)))
  "Create a texture-entity for THING."
  (ctypecase thing
    (string (create-texture-entity-for-string thing color))
    (base-char (create-texture-entity-for-string (string thing) color))
    (symbol (create-texture-entity-for-notation-glyph thing color))
    (integer (create-texture-entity-for-integer thing color))))

(defun get-texture-entity-for (thing)
  "Get a texture-entity for THING from cache or create and store it in the
cache it if it's not already in the cache."
  (or (gethash thing *gl-texture-entity-cache*)
      (setf (gethash thing *gl-texture-entity-cache*)
            (create-texture-entity-for thing))))

(defun create-drawing-command-for-staff (width clef clef-pos)
  "Create code for drawing a staff of WIDTH, having the CLEF at
  position CLEF-POS (notehead heights from the lowest staff line) and
  KEY. The returned code must be called with x and y coordinates."
  (lambda (x y)
    (gl:with-pushed-matrix
      (gl:translate x y 0)
      (let ((clef-texture-entity (get-texture-entity-for clef)))
        (dotimes (i 5)
          (draw-horizontal-line 0 width (* i *notehead-height*)
                                *staff-line-width* '(0 0 0)))
        (draw-texture-entity clef-texture-entity
                             0 (* *notehead-height* clef-pos)
                             nil)))))

(defclass graphics-element ()
  ((pos
    :accessor pos :initarg :pos :initform (cons 0 0)
    :documentation "Position in screen coordinate system of this
    graphics element. Cons of x and y coordinates. Origo is in lower
    left corner.")
   (parent-element
    :accessor parent-element :initarg :parent-element :initform nil
    :documentation "Parent element for this element. This element will
    be drawn with position relative to the parent-element.")
   (velocity
    :accessor velocity :initarg :velocity :initform (cons 0 0)
    :documentation "Velocity in x and y direction. A cons of x and y
    velocity values. Screen pixels per frame. Positive values
    mean up and rightwards.")
   (drawing-primitives
    :accessor drawing-primitives :initarg :drawing-primitives
    :documentation "A list of drawing-primitives to draw this
    element.")
   (color
    :accessor color :initarg :color :initform (list 0 0 0 1)
    :documentation "Color of this element. A list of 4 values RGBA
    in range of [0-1]")
   (lifetime
    :accessor lifetime :initarg :lifetime :initform nil
    :documentation "The lifetime of this element. Value of nil means
    forever. This is updated from outside.")
   (effects
    :accessor effects :initarg :effects :initform nil
    :documentation "If non nil; assumed to be a function that's called
    every time just before drawing, for creating some effects."))
  (:documentation "A drawable element, consisting of possibly many
  texture-entities and other drawing primitives."))

(defun color-fade-effect (inst &key (r #'identity) (g #'identity)
                                 (b #'identity) (a #'identity))
  "Fade (or multiply) the color of INST as specified by each key
argument. Returns the new color."
  (setf (color inst)
        (loop :for c in (color inst)
           :for fun :in (list r g b a)
           :collect (funcall fun c))))

(defmacro cons-op (op cons1 cons2)
  "Do the OP between elements of CONS1 and CONS2. Return the result in
a new cons."
  (let ((op-s (gensym "op"))
        (cons1-s (gensym "cons1"))
        (cons2-s (gensym "cons2")))
    `(let ((,op-s ,op)
           (,cons1-s ,cons1)
           (,cons2-s ,cons2))
       (cons (funcall ,op-s (car ,cons1-s) (car ,cons2-s))
             (funcall ,op-s (cdr ,cons1-s) (cdr ,cons2-s))))))

(defun disassemble-graphics-element (elem)
  "Disassemble a graphics element ELEM consisting of multiple
  pieces (the drawing primitives) into its indivisible parts, which
  retain the relative position. Return a list of these indivisible
  graphics elements."
  (with-slots (pos parent-element velocity drawing-primitives color lifetime effects)
      elem
    (let ((primitive-pieces (disentangle-drawing-primitives drawing-primitives))
          indivisible-elements)
      (dolist (pos-and-primitive primitive-pieces indivisible-elements)
        (destructuring-bind ((ppx . ppy) prim-drawing-cmds) pos-and-primitive
          (let ((prim-pos-screen-coords
                 (cons (* ppx *notehead-width*)
                       (* ppy *notehead-height*))))
            (push
             (make-instance
              'graphics-element
              :pos (cons-op '+ prim-pos-screen-coords pos)
              :parent-element parent-element :velocity (copy-list velocity)
              :drawing-primitives prim-drawing-cmds
              :color (copy-list color) :lifetime lifetime :effects effects)
             indivisible-elements)))))))

(defgeneric draw (inst)
  (:documentation "Draw instance INST to screen"))

(defmethod draw ((inst graphics-element))
  (let ((effective-coord
         (if (parent-element inst)
             (cons-op '+ (pos inst) (pos (parent-element inst)))
             (pos inst)))
        (effect-fun (effects inst)))
    (gl:with-pushed-matrix
      (gl:translate (car effective-coord) (cdr effective-coord) 0)
      (when effect-fun
        (funcall effect-fun inst))
      (apply #'gl:color (color inst))
      (draw-graphics (drawing-primitives inst)))))

(defclass graphics-chord (graphics-element notational-chord)
  ()
  (:documentation "A chord."))

(defclass graphics-staff (graphics-element notational-staff)
  ((width
    :accessor width :initarg :width
    :documentation "Width of this staff.")
   (drawing-command
    :reader drawing-command
    :documentation "A function to call to draw this staff on the
    screen. Takes x and y coordinates as parameters. These are screen
    coordinates."))
  (:documentation "Drawable staff."))

(defmethod draw ((inst graphics-staff))
  (let ((effect-fun (effects inst)))
    (gl:with-pushed-matrix
      (when effect-fun
        (funcall effect-fun inst))
      (destructuring-bind (x . y) (pos inst)
        (funcall (drawing-command inst) x y)))))

(defmethod initialize-instance :after ((staff graphics-staff) &key)
  (setf (slot-value staff 'drawing-command)
        (create-drawing-command-for-staff (width staff) (clef staff) (get-clef-pos staff))))

(defun detach-elem-from-paren (elem)
  "Detach the ELEM from its parent. Retain the ELEM's relative
position."
  (let* ((parent (parent-element elem))
         (paren-pos (pos parent))
         (paren-vel (velocity parent)))
    (setf (parent-element elem) nil
          (pos elem) (cons-op '+ paren-pos (pos elem))
          (velocity elem) (cons-op '+ paren-vel (velocity elem)))))

;;; TODO: Implement, for key change during game
;; (defmethod (setf key-signature) :after (new-key (inst graphics-staff))
;;   "Handle additional tasks when setting NEW-KEY as the new key
;; signature of the staff INST."
;;   (setf (slot-value inst 'drawing-command)
;;         (create-drawing-command-for-staff (width staff) (clef staff) (get-clef-pos staff))))

(defmethod (setf clef) :after (new-clef (inst graphics-staff))
  "Handle additional tasks when setting NEW-CLEF as the new clef of
the staff INST."
  (declare (optimize (debug 3)))
  (setf (slot-value inst 'drawing-command)
        (create-drawing-command-for-staff (width inst) new-clef
                                          (get-clef-pos inst))))

(defun draw-primitive (drawing-primitive)
  "Draw the DRAWING-PRIMITIVE on the screen"
  (case (first drawing-primitive)
    ((notehead ♯ ♭ ♮)
     (draw-texture-entity
      (get-texture-entity-for (first drawing-primitive)) 0 0))
    (rotate
     (gl:rotate (second drawing-primitive) 0 0 1))
    (ledger-line
     (let* ((len (* (second drawing-primitive) *notehead-width*))
            (x1 (/ len -2))
            (x2 (/ len 2)))
       (draw-horizontal-line x1 x2 0 *staff-line-width*)))
    (stem
     (let* ((height (* (second drawing-primitive) *notehead-height*))
            (y1 (/ height -2))
            (y2 (/ height 2)))
       (draw-vertical-line 0 y1 y2 *stem-width*)))
    (translate
     (gl:translate (* (second drawing-primitive) *notehead-width*)
                   (* (third drawing-primitive) *notehead-height*) 0))
    (save-transformation
     (gl:with-pushed-matrix
       (draw-graphics (cdr drawing-primitive))))))

(defun draw-graphics (drawing-primitives)
  "Draw the list of DRAWING-PRIMITIVES on the screen"
  (dolist (primitive drawing-primitives)
    (draw-primitive primitive)))

(defun create-chord (mkns staff)
  "Create a chord from MKNs which can be drawn over STAFF."
  (let ((inst (make-instance
               'graphics-chord
               :mkn-list mkns
               :color (list 0 0 0 1)
               :drawing-primitives (create-drawing-primitives-for-chord mkns staff)
               :velocity (cons 0 0)
               :parent-element staff
               :pos (cons (width staff) 0))))
    inst))

(defun draw-number-digits (digit-seq x y color)
  "Draw a sequence of digits (including decimal dot) to screen,
  starting from coordinate X, Y. Set color first to COLOR."
  (let ((effective-x x)
        (total-bb-width 0))
    (gl:with-pushed-matrix
      (apply 'gl:color color)
      (loop :for elem :across digit-seq
         :for texture-entity = (get-texture-entity-for
                                (or (digit-char-p elem) elem))
         :for bb = (bounding-box texture-entity)
         :for bb-width = (bb-width bb)
         ;; Subtract the left x-offset of the element (aref bb 0),
         ;; since some glyphs in the font have huge left offsets and
         ;; things will look funny without doing this. TODO: I know
         ;; nothing about fonts, this is probably not the right way to
         ;; do this.
         :do (progn (draw-texture-entity texture-entity
                                         (- effective-x (aref bb 0)) y nil)
                    (incf effective-x bb-width)
                    (incf total-bb-width bb-width))))
    total-bb-width))

;;; Note that for now, all strings and numbers are drawn with one
;;; fixed font size.
(defun draw-number (value x y &optional (color (list 0 0 0 1)))
  "Draw a number (integer or float) with specified VALUE to the
   specified coordinates X and Y (real screen coordinates) with the
   given COLOR. Return the width of the drawn string.

   Numbers are drawn a digit at a time, since if they were drawn as a
   string, a new texture would have to be created for every distinct
   number. This would be slow and require a lot of memory."
  (ctypecase value
    (integer (draw-number-digits (format nil "~d" value) x y color))
    (float (draw-number-digits (format nil "~,2f" value) x y color))))

(defun draw-string (string x y &optional (color (list 0 0 0 1)))
  "Draw a STRING to the specified coordinates X and Y (real screen
  coordinates) with the given COLOR. Return the width of the drawn
  string."
  (let ((texture-entity (get-texture-entity-for string)))
    (gl:with-pushed-matrix
      (apply 'gl:color color)
      (draw-texture-entity (get-texture-entity-for string) x y nil nil))
    (bb-width (bounding-box texture-entity))))
