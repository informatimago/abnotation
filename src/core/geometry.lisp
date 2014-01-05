;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               geometry.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Geometry: coordinates, points, rectangle and ranges.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-12-09 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2013 - 2013
;;;;    
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************

(in-package "ABNOTATION.CORE")


;;; Cocoa uses the normal mathematical coordinates system, with the
;;; origin on the bottom-left corner, and Y axis going upward.
;;;
;;; We use the same coordinate systems, with floating-point numbers,
;;; in units of millimeter.

;;;------------------------------------------------------------
;;;
;;; Representation of coordinates, points, sizes and rectangles.
;;;


;;;---------------------------------------------------------------------
;;; Generic geometry
;;;---------------------------------------------------------------------

(defun square (x)
  "Returns the argument to the power of 2."
  (* x x))
(declaim (inline square))


(defconstant 2pi   (* pi 2))
(defconstant pi/2  (/ pi 2))
(defconstant 2pi/360 (* 2/360 pi))

(defun radian (degree) (* degree 2pi/360))
(defun degree (radian) (/ radian 2pi/360))
(declaim (inline degree radian))


(defgeneric right  (object))
(defgeneric left   (object))
(defgeneric bottom (object))
(defgeneric top    (object))
(defgeneric width  (object))
(defgeneric height (object))

(defgeneric (setf right)  (new-value object))
(defgeneric (setf left)   (new-value object))
(defgeneric (setf bottom) (new-value object))
(defgeneric (setf top)    (new-value object))
(defgeneric (setf width)  (new-value object))
(defgeneric (setf height) (new-value object))

(defgeneric origin (object)
  (:documentation "The point origin of the coordinates of the ``OBJECT``."))

(defgeneric (setf origin) (new-value object)
  (:documentation "Change the origin of the ``OBJECT``."))

(defgeneric extent (object)
  (:documentation "The size of the ``OBJECT``."))

(defgeneric (setf extent) (new-value object)
  (:documentation "Change the size of the ``OBJECT``."))

(defgeneric bounds (object)
  (:documentation "
The rectangle surrounding the ``OBJECT``, in the coordinate system
relative to the ``ORIGIN``.
"))

(defgeneric frame (object)
  (:documentation "
The rectangle surrounding the ``OBJECT``, in the coordinate system
where the object is drawn (same coordinate system in which ``ORIGIN`` is
expressed). ::

    (frame object) == (rect-offset (bounds object)
                                   (point-x (origin object))
                                   (point-y (origin object)))

")
  (:method (object)
    (rect-offset (bounds object)
                 (point-x (origin object))
                 (point-y (origin object)))))

(defgeneric place (object point)
  (:documentation "Change the origin of the ``OBJECT`` to be the ``POINT``.")
  (:method (object (to ns:ns-point))
    (setf (origin object) to)
    object))


(defgeneric above (object &optional offset)
  (:documentation "Returns a point that is above the OBJECT by OFFSET units.")
  (:method (object &optional (offset 0)) (point (left object) (+ (top object) offset))))
(defgeneric below (object &optional offset)
  (:documentation "Returns a point that is below the OBJECT by OFFSET units.")
  (:method (object &optional (offset 0)) (point (left object) (- (bottom object) offset))))
(defgeneric left-of (object &optional offset)
  (:documentation "Returns a point that is on the left of the OBJECT by OFFSET units.")
  (:method (object &optional (offset 0)) (point (- (left object) offset) (bottom object))))
(defgeneric right-of (object &optional offset)
  (:documentation "Returns a point that is on the right of the OBJECT by OFFSET units.")
  (:method (object &optional (offset 0)) (point (+ (right object) offset) (bottom object))))



(defgeneric vector-x (a))
(defgeneric vector-y (a))
(defgeneric vector+ (a b)
  (:documentation "Returns an object representing the vectorial sum of A and B."))
(defgeneric vector- (a b)
  (:documentation "Returns an object representing the vectorial difference of A and B."))
(defgeneric vector* (s v)
  (:documentation "Returns an object representing the scalar product of the scalar S with the vector V"))
(defgeneric rotate (angle v)
  (:documentation "Returns the rotation of the vector V by the angle ANGLE."))
(defgeneric dot-product (a b)
  (:documentation "Returns the dot product of the vectors A and B."))
(defgeneric determinant (a b)
  (:documentation "Returns the determinant of the matrix [A B]."))
(defgeneric vector-angle (a b)
  (:documentation "Returns the angle from A to B.")
  (:method (a b)
    (let ((ac (acos (/ (dot-product a b) (vector-abs a) (vector-abs b)))))
      (if (minusp (determinant a b))
          (- 2pi ac)
          ac))))
(defgeneric vector-abs (v)
  (:documentation "Returns the module of the vector V."))
(defgeneric unit-vector (v)
  (:documentation "Returns a vector colinear to V but with a length of 1.")
  (:method (v) (vector* (/ (vector-abs v)) v)))


(defun line-intersection (p1 v1 p2 v2)
  "
p1+u1*v1 = p2+u2*v2
u1*v1 - u2*v2 = p2-p1 = v0
u1 =   det[v0,v2]/det[v1,v2]
u2 = - det[v0,v1]/det[v1,v2]
"
  (let* ((v0  (vector- p2 p1))
         (d1  (determinant v0 v2))
         (d0  (determinant v1 v2)))
    (and (not (zerop d0))
         (vector+ p1 (vector* (/ d1 d0) v1)))))



;;;---------------------------------------------------------------------
;;; COORDINATE
;;;---------------------------------------------------------------------

(deftype coordinate ()
  (or (and (find-package "NS") (find-symbol "CGFLOAT" "NS"))  ;; can be 32-bit or 64-bit float.
      'single-float))
(setf *read-default-float-format* 'coordinate)
(defun coordinate (value) (coerce value 'coordinate))
(declaim (inline coordinate))


;;;---------------------------------------------------------------------
;;; POINT
;;;---------------------------------------------------------------------

(defstruct (point (:constructor %make-point))
  (x      (coordinate 0.0d0) :type coordinate)
  (y      (coordinate 0.0d0) :type coordinate))

(defun make-point (&key (x 0.0d0) (y 0.0d0))
  (%make-point :x (coordinate x) :y (coordinate y)))
(defun point (x y) (make-point :x x :y y))
(declaim (inline make-point point))

(defmethod right  ((p point)) (point-x p))
(defmethod left   ((p point)) (point-x p))
(defmethod bottom ((p point)) (point-y p))
(defmethod top    ((p point)) (point-y p))
(defmethod width  ((p point)) 0.0)
(defmethod height ((p point)) 0.0)
(defmethod origin ((p point)) p)
(defmethod extent ((p point)) (size 0.0 0.0))
(defmethod (setf left) (new-value (p point))
  (setf (point-x p) new-value))
(defmethod (setf right) (new-value (p point))
  (setf (point-x p) new-value))
(defmethod (setf top) (new-value (p point))
  (setf (point-y p) new-value))
(defmethod (setf bottom) (new-value (p point))
  (setf (point-y p) new-value))
(defmethod (setf origin) (new-value (p point))
  (setf (point-x p) (point-x new-value)
        (point-y p) (point-y new-value))
  new-value)
(defmethod bounds ((p point)) (rect 0.0 0.0 0.0 0.0))
(defmethod frame ((p point)) (make-rect :origin p :width 0.0 :height 0.0))

;;;---------------------------------------------------------------------
;;; SIZE
;;;---------------------------------------------------------------------

(defstruct (size (:constructor %make-size))
  (width  (coordinate 0.0d0) :type coordinate)
  (height (coordinate 0.0d0) :type coordinate))

(defun make-size (&key (width 0.0d0) (height 0.0d0))
  (%make-size :width (coordinate width) :height (coordinate height)))
(defun size (width height) (make-size :width width :height height))
(declaim (inline make-size size))

(defmethod width  ((s size)) (size-width  s))
(defmethod height ((s size)) (size-height s))
(defmethod extent ((s size)) s)
(defmethod bounds ((s size)) (make-rect :x 0.0 :y 0.0 :size s))

(defmethod (setf width) (new-value (s size))
  (setf (size-width s) new-value))
(defmethod (setf height) (new-value (s size))
  (setf (size-height s) new-value))

;;;---------------------------------------------------------------------
;;; RECT
;;;---------------------------------------------------------------------

(defstruct (rect (:constructor %make-rect))
  (x      (coordinate 0.0d0) :type coordinate)
  (y      (coordinate 0.0d0) :type coordinate)
  (width  (coordinate 0.0d0) :type coordinate)
  (height (coordinate 0.0d0) :type coordinate))


(defun make-rect (&key (x 0.0d0 xp) (y 0.0d0 yp) (width 0.0d0 widthp) (height 0.0d0 heightp)
                    ;; (left 0.0d0 leftp)
                    ;; (right 0.0d0 rightp)
                    ;; (bottom 0.0d0 bottomp)
                    ;; (top 0.0d0 topp)
                    origin size)
  (assert (xor (or xp yp) origin))
  (assert (xor (or widthp heightp) size))
  (if origin
      (if size
          (%make-rect :x     (point-x origin)    :y      (point-y origin)
                      :width (size-width size)   :height (size-height size))
          (%make-rect :x     (point-x origin)    :y      (point-y origin)
                      :width (coordinate width)  :height (coordinate height)))
      (if size
          (%make-rect :x     (coordinate x)      :y      (coordinate y)
                      :width (size-width size)   :height (size-height size))
          (%make-rect :x     (coordinate x)      :y      (coordinate y)
                      :width (coordinate width)  :height (coordinate height)))))
(defun rect (x y width height)
  (%make-rect :x (coordinate x)
              :y (coordinate y)
              :width (coordinate width)
              :height (coordinate height)))
(declaim (inline make-rect rect))


(defun rect-left     (r) (rect-x r))
(defun rect-right    (r) (+ (rect-x r) (rect-width r)))
(defun rect-bottom   (r) (rect-y r))
(defun rect-top      (r) (+ (rect-x r) (rect-height r)))
(defun rect-center-x (r) (+ (rect-x r) (/ (rect-width r) 2)))
(defun rect-center-y (r) (+ (rect-y r) (/ (rect-height r) 2)))
(defun rect-origin   (r) (point (rect-x r) (rect-y r)))
(defun rect-size     (r) (size (rect-width r) (rect-height r)))
(defun (setf rect-origin) (point rect)
  (setf (rect-x rect) (point-x point)
        (rect-y rect) (point-y point))
  point)
(defun (setf rect-size) (size rect)
  (setf (rect-width  rect) (size-width  size)
        (rect-height rect) (size-height size))
  size)
(declaim (inline rect-left rect-right rect-bottom rect-top rect-center-x rect-center-y
                 rect-origin rect-size rect-to-list))

(defmethod right  ((r rect)) (rect-right  r))
(defmethod left   ((r rect)) (rect-left   r))
(defmethod bottom ((r rect)) (rect-bottom r))
(defmethod top    ((r rect)) (rect-top    r))
(defmethod width  ((r rect)) (rect-width  r))
(defmethod height ((r rect)) (rect-height r))
(defmethod origin ((r rect)) (rect-origin r))
(defmethod extent ((r rect)) (rect-size   r))

(defmethod (setf left) (new-value (r rect))
  (setf (rect-x r) (coordinate new-value)))
(defmethod (setf bottom) (new-value (r rect))
  (setf (rect-y r) (coordinate new-value)))

(defmethod (setf right) (new-value (r rect))
  (setf (rect-width r) (coordinate (- new-value (left r))))
  new-value)
(defmethod (setf top) (new-value (r rect))
  (setf (rect-height r) (coordinate (- new-value (bottom r))))
  new-value)

(defmethod (setf width) (new-value (r rect))
  (setf (rect-width r) (coordinate new-value)))
(defmethod (setf height) (new-value (r rect))
  (setf (rect-height r) (coordinate new-value)))

(defmethod (setf origin) (new-value (r rect))
  (setf (rect-x r) (point-x new-value)
        (rect-y r) (point-y new-value))
  new-value)

(defmethod (setf extent) (new-value (r rect))
  (setf (rect-width r) (size-width new-value)
        (rect-height r) (size-height new-value))
  new-value)

(defmethod bounds ((r rect)) (rect 0.0 0.0 (width r) (height r)))
(defmethod frame ((r rect)) r)
(defmethod (setf frame) ((new-value rect) (r rect))
  (setf (rect-x r) (rect-x new-value)
        (rect-y r) (rect-y new-value)
        (rect-width r) (rect-width new-value)
        (rect-height r) (rect-height new-value))
  new-value)

(defun rect-to-list (rect)
  (list (rect-x rect)
        (rect-y rect)
        (rect-width rect)
        (rect-height rect)))

(defun rect-inset (r inset-x inset-y)
  "Returns a rect like R but with the borders \"inset\" by inset-x horizontaly, and by inset-y vertically.
If inset-x/y is positive the result is smaller, if it's negative, then the result is larger."
  (rect  (+ (rect-x r) inset-x)
         (+ (rect-y r) inset-y)
         (- (rect-width r) inset-x inset-x)
         (- (rect-height r) inset-y inset-y)))


(defun rect-offset (r dx dy)
  "Returns a rect of same size as ``R`` but with an origin offset by ``DX`` and ``DY``."
  (rect  (+ dx (rect-x r))
         (+ dy (rect-y r))
         (rect-width r)
         (rect-height r)))

(defun rect-union (a b)
  "Returns the smallest rect that covers both the rects ``A`` and ``B``."
  (let ((x  (min (left a) (left b)))
        (y  (min (bottom a) (bottom b))))
    (rect  x y
           (- (max (right a) (right b)) x)
           (- (max (top   a) (top   b)) y))))


(defun rect-empty-p (r)
  (or (minusp (width r))
      (minusp (height r))))


(defun rect-intersection (a b)
  (if (or (< (right a) (left b))
          (< (right b) (left a))
          (< (top a) (bottom b))
          (< (top b) (bottom a)))
    (rect 0 0 -1 -1)
    (let ((x (max (left a) (left b)))
          (y (max (bottom a) (bottom b))))
      (rect x y
            (- (min (right a) (right b)) x)
            (- (min (top a) (top b)) y)))))

(defmethod left-side   (object &optional (thickness 1))
  (rect (left  object) (bottom object) thickness (height object)))
(defmethod right-side  (object &optional (thickness 1))
  (rect (right object) (bottom object) thickness (height object)))
(defmethod bottom-side (object &optional (thickness 1))
  (rect (left  object) (bottom object) (width object) thickness))
(defmethod top-side    (object &optional (thickness 1))
  (rect (left  object) (top    object) (width object) thickness))


(defun rect-expand (rect point)
  "Modify the RECT so that it includes the POINT."
  (let ((left   (min (left   rect) (point-x point)))
        (bottom (min (bottom rect) (point-y point)))
        (right  (max (right  rect) (point-x point)))
        (top    (max (top    rect) (point-y point))))
    (setf (rect-x      rect) left
          (rect-y      rect) bottom
          (rect-width  rect) (- right left)
          (rect-height rect) (- top bottom))
    rect))




;;;---------------------------------------------------------------------
;;; RANGE
;;;---------------------------------------------------------------------

(defstruct (range (:constructor %make-range))
  (location 0 :type unsigned-byte)
  (length   0 :type unsigned-byte))

(defun make-range (&key (location 0) (length 0))
  (%make-range :location (truncate location) :length (truncate length)))
(defun range (location length) (make-range :location location :length length))
(declaim (inline make-range range))


;;;---------------------------------------------------------------------
;;; POINT as vectors
;;;---------------------------------------------------------------------

(defun point= (a b)
  (or (eql a b)
      (and (= (point-x a) (point-x b))
           (= (point-y a) (point-y b)))))

(defmethod above ((self point) &optional (offset 0))
  (point (point-x self) (+ (point-y self) offset)))

(defmethod below ((self point) &optional (offset 0))
  (point (point-x self) (- (point-y self) offset)))

(defmethod left-of ((self point) &optional (offset 0))
  (point (- (point-x self) offset) (point-y self)))

(defmethod right-of ((self point) &optional (offset 0))
  (point (+ (point-x self) offset) (point-y self)))


(defmethod vector-x ((a point)) (point-x a))
(defmethod vector-y ((a point)) (point-y a))

(defmethod vector+ ((a point) (b size))
  (point (+ (point-x a) (size-width  b))
         (+ (point-y a) (size-height b))))

(defmethod vector- ((a point) (b point))
  (size (- (point-x a) (point-x b))
        (- (point-y a) (point-y b))))

(defmethod vector* ((a real) (b point))
  (point (* a (point-x b))
         (* a (point-y b))))

(defmethod rotate ((angle real) (v point))
  (let ((s (sin angle))
        (c (cos angle))
        (x (point-x v))
        (y (point-y v)))
    (point (- (* c x) (* s y))
           (+ (* s x) (* c y)))))

(defmethod dot-product ((a point) (b point))
  (+ (* (point-x a) (point-x b))
     (* (point-y a) (point-y b))))

(defmethod determinant ((a point) (b point))
  (- (* (point-x a) (point-y b))
     (* (point-y a) (point-x b))))

(defmethod vector-abs ((self point))
  (sqrt (+ (square (point-x self))
           (square (point-y self)))))

(defmethod vector-rotate ((self point) angle)
  (let ((s (sin angle))
        (c (cos angle))
        (x (point-x self))
        (y (point-y self)))
    (point (- (* c x) (* s y))
           (+ (* s x) (* c y)))))



(defmethod distance-squared ((p point) (q point))
  (+ (square (- (point-x p) (point-x q)))
     (square (- (point-y p) (point-y q)))))

;;;---------------------------------------------------------------------
;;; SIZE as vectors
;;;---------------------------------------------------------------------

(defun size= (a b)
  (or (eql a b)
      (and (= (size-width  a) (size-width  b))
           (= (size-height a) (size-height b)))))

(defmethod vector-x ((a size)) (size-width  a))
(defmethod vector-y ((a size)) (size-height a))

(defmethod vector+ ((a size) (b size))
  (size (+ (size-width  a) (size-width  b))
        (+ (size-height a) (size-height b))))

(defmethod vector- ((a size) (b size))
  (size (- (size-width  a) (size-width  b))
        (- (size-height a) (size-height b))))

(defmethod vector* ((a real) (b size))
  (size (* a (size-width  b))
        (* a (size-height b))))

(defmethod dot-product ((a size) (b size))
  (+ (* (size-width  a) (size-width  b))
     (* (size-height a) (size-height b))))

(defmethod rotate ((angle real) (v size))
  (let ((s (sin angle))
        (c (cos angle))
        (x (size-width v))
        (y (size-height v)))
    (size (- (* c x) (* s y))
          (+ (* s x) (* c y)))))

(defmethod determinant ((a size) (b size))
  (- (* (size-width  a) (size-height b))
     (* (size-height a) (size-width  b))))

(defmethod vector-abs ((self size))
  (sqrt (+ (square (size-width  self))
           (square (size-height self)))))

(defmethod vector-rotate ((self size) angle)
  (let ((s (sin angle))
        (c (cos angle))
        (x (size-width self))
        (y (size-height self)))
    (size (- (* c x) (* s y))
          (+ (* s x) (* c y)))))


;;;---------------------------------------------------------------------

(defun test/line-intersection ()
 (assert
  (equalp
   (list (line-intersection (point  10 10) (size  2 2) (point  1  0) (size -3 3))
         (line-intersection (point  10 10) (size  2 2) (point  0 -1) (size -3 3))
         (line-intersection (point -10 10) (size -2 2) (point -1  0) (size  3 3))
         (line-intersection (point -10 10) (size -2 2) (point  0  1) (size  3 3)))
   (list (POINT  1/2  1/2)
         (POINT -1/2 -1/2)
         (POINT -1/2  1/2)
         (POINT -1/2  1/2)))))


;;;---------------------------------------------------------------------

(defun stack-objects (objects &key (direction :up) (align :left) (spacing 0))
  "
Stack up or down the ``OBJECTS`` based on the position of the first one.
"
  (when objects
    (let* ((frame (frame (first objects)))
           (x  (ecase align
                 (:left   (rect-left     frame))
                 (:right  (rect-right    frame))
                 (:center (rect-center-x frame))))
           (y  (ecase direction
                 (:up   (rect-top    frame))
                 (:down (rect-bottom frame)))))
      (loop
         :for object :in (rest objects)
         :for frame = (frame object)
         :do (when (eq direction :down)
               (decf y (+ spacing (rect-height frame))))
         :do (place object (ecase align
                             (:left   (make-point :x x                              :y y))
                             (:right  (make-point :x (- x (rect-width frame))       :y y))
                             (:center (make-point :x (- x (/ (rect-width frame) 2)) :y y))))
         :do (when (eq direction :up)
               (incf y (+ spacing (rect-height frame)))))))
  objects)

(defun stack-up (objects &key (align :left) (spacing 0))
  (stack-objects objects :direction :up :align align :spacing spacing))

(defun pile-down (objects &key (align :left) (spacing 0))
  (stack-objects objects :direction :down :align align :spacing spacing))

;;;---------------------------------------------------------------------


(defun test/tlbr/rect ()
  (let ((r (rect 0 0 0 0)))
    (setf (bottom r) 24)
    (assert (= (bottom r) 24))
    (setf (left r) 24)
    (assert (= (left r) 24))
    (setf (top r) 42)
    (assert (= (top r) 42))
    (setf (right r) 42)
    (assert (= (right r) 42)))
  :success)

(test/tlbr/rect)



(defmethod print-object ((p point) stream)
  (format stream "(point ~,2F ~,2F)" (point-x p) (point-y p))
  p)

(defmethod print-object ((s size) stream)
  (format stream "(size ~,2F ~,2F)" (size-width s) (size-height s))
  s)

(defmethod print-object ((r rect) stream)
  (format stream "(rect ~,2F ~,2F ~,2F ~,2F)"
          (rect-x r) (rect-y r)
          (rect-width r) (rect-height r))
  r)



;;;; THE END ;;;;
