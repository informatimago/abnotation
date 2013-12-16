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

(defun xor (a b)
  "Return A ⊻ B"
  (or (and a (not b)) (and (not a) b)))


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

(defmethod right  ((p point)) (point-x p))
(defmethod left   ((p point)) (point-x p))
(defmethod bottom ((p point)) (point-y p))
(defmethod top    ((p point)) (point-y p))

;;;---------------------------------------------------------------------
;;; SIZE
;;;---------------------------------------------------------------------

(defstruct (size (:constructor %make-size))
  (width  (coordinate 0.0d0) :type coordinate)
  (height (coordinate 0.0d0) :type coordinate))

(defun make-size (&key (width 0.0d0) (height 0.0d0))
  (%make-size :width (coordinate width) :height (coordinate height)))
(defun size (width height) (make-size :width width :height height))


;;;---------------------------------------------------------------------
;;; RECT
;;;---------------------------------------------------------------------

(defstruct (rect (:constructor %make-rect))
  (x      (coordinate 0.0d0) :type coordinate)
  (y      (coordinate 0.0d0) :type coordinate)
  (width  (coordinate 0.0d0) :type coordinate)
  (height (coordinate 0.0d0) :type coordinate))

(defun rect-left   (r) (rect-x r))
(defun rect-right  (r) (+ (rect-x r) (rect-width r)))
(defun rect-bottom (r) (rect-y r))
(defun rect-top    (r) (+ (rect-x r) (rect-height r)))
(defun rect-center-x (r) (+ (rect-x r) (/ (rect-width r) 2)))
(defun rect-center-y (r) (+ (rect-y r) (/ (rect-height r) 2)))
(declaim (inline rect-left rect-right rect-bottom rect-top rect-center-x rect-center-y))

(defmethod right  ((r rect)) (rect-right  r))
(defmethod left   ((r rect)) (rect-left   r))
(defmethod bottom ((r rect)) (rect-bottom r))
(defmethod top    ((r rect)) (rect-top    r))

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
(defun rect (x y width height) (make-rect :x x :y y :width width :height height))

(defun rect-origin (rect)
  (%make-point :x     (rect-x rect)     :y      (rect-y rect)))
(defun rect-size   (rect)
  (%make-size  :width (rect-width rect) :height (rect-height rect)))

(defun (setf rect-origin) (point rect)
  (setf (rect-x rect) (point-x point)
        (rect-y rect) (point-y point)))
(defun (setf rect-size)   (size  rect)
  (setf (rect-width  rect) (size-width  size)
        (rect-height rect) (size-height size)))

(defun rect-to-list (rect)
  (list (rect-x rect)
        (rect-y rect)
        (rect-width rect)
        (rect-height rect)))



(defun rect-offset (r dx dy)
  "Returns a rect of same size as ``R`` but with an origin offset by ``DX`` and ``DY``."
  (make-rect  (+ dx (rect-x r))
              (+ dy (rect-y r))
              (rect-width r)
              (rect-height r)))

(defun rect-union (a b)
  "Returns the smallest rect that covers both the rects ``A`` and ``B``."
  (let ((x  (min (left a) (left b)))
        (y  (min (bottom a) (bottom b))))
    (make-rect  x y
                (- (max (right a) (right b)) x)
                (- (max (top   a) (top   b)) y))))


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


;;;; THE END ;;;;
