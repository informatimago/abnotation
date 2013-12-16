;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               bezier.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines paths.
;;;;
;;;;    For speed while defining and processing paths, we implement them in lisp.
;;;;    Each backend will translate paths into native paths when drawing them.
;;;;
;;;;    Paths are essentially a sequence of path-elements, which are either:
;;;;    move-to-point, line-to-point, quad-curve-to-point and curve-to-point.
;;;;    (quadratic Bézier curves and cubic Bézier curves).
;;;;
;;;;    Subpaths are defined by subsequences from a move-to-point to the
;;;;    next.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-12-16 <PJB> Adapted to abnotation.
;;;;    2011-05-12 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2011 - 2013
;;;;    
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Affero General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Affero General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU Affero General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************

(in-package "ABNOTATION.CORE")


(defclass path-element ()
  ()
  (:documentation "A path element.

This abstract class has subclasses corresponding to each kind of path
element.

Path elements are simple path primitives: move-to-point,
line-to-point, quad-curve-to-point and curve-to-point.  The last two
represent Bézier curves of degree 2 and 3.

Paths and subpaths always start with a move-to-point element, and a
move-to-point element defines the beginning of a new subpath."))

(defclass path-element-move-to-point       (path-element)
  ((point           :initarg :point           :reader path-element-point))
  (:documentation "Sets the new current point of the path."))

(defclass path-element-line-to-point       (path-element)
  ((point           :initarg :point           :reader path-element-point))
  (:documentation "Defines a line from the current point to the new point."))

(defclass path-element-quad-curve-to-point (path-element)
  ((point           :initarg :point           :reader path-element-point)
   (control-point   :initarg :control-point   :reader path-element-control-point))
  (:documentation "Defines a quadratic Bézier curve with the current point,
the control-point and the new point."))

(defclass path-element-curve-to-point      (path-element)
  ((point           :initarg :point           :reader path-element-point)
   (control-point-1 :initarg :control-point-1 :reader path-element-control-point-1)
   (control-point-2 :initarg :control-point-2 :reader path-element-control-point-2))
  (:documentation "Defines a Bézier curve of order 3 with the current point,
the control-point-1, the control-point-2 and the new point.

The methods adding elements to a path come usually in two flavor:
with POINT parameters, and with X, and Y coordinates parameters.
"))



(defmethod print-object ((self path-element-move-to-point) stream)
  (print `(move-to-point path transform (point ,(point-x (path-element-point self))
                                               ,(point-y (path-element-point self))))
         stream))

(defmethod print-object ((self path-element-line-to-point) stream)
  (print `(line-to-point path transform (point ,(point-x (path-element-point self))
                                               ,(point-y (path-element-point self))))
         stream))

(defmethod print-object ((self path-element-quad-curve-to-point) stream)
  (print `(quad-curve-to-point path transform
                               (point ,(point-x (path-element-control-point self))
                                      ,(point-y (path-element-control-point self)))
                               (point ,(point-x (path-element-point self))
                                      ,(point-y (path-element-point self))))
         stream))

(defmethod print-object ((self path-element-curve-to-point) stream)
  (print `(curve-to-point path transform
                          (point ,(point-x (path-element-control-point-1 self))
                                 ,(point-y (path-element-control-point-1 self)))
                          (point ,(point-x (path-element-control-point-2 self))
                                 ,(point-y (path-element-control-point-2 self)))
                          (point ,(point-x (path-element-point self))
                                 ,(point-y (path-element-point self))))
         stream))


(defgeneric copy-path-element (element)
  (:documentation "Returns a copy of the element.")
  (:method ((element path-element-move-to-point))
    (make-instance (class-of element)
        :point (path-element-point element)))
  (:method ((element path-element-line-to-point))
    (make-instance (class-of element)
        :point (path-element-point element)))
  (:method ((element path-element-quad-curve-to-point))
    (make-instance (class-of element)
        :point         (path-element-point element)
        :control-point (path-element-control-point element)))
  (:method ((element path-element-curve-to-point))
    (make-instance (class-of element)
        :point           (path-element-point element)
        :control-point-1 (path-element-control-point-1 element)
        :control-point-2 (path-element-control-point-2 element))))


(defgeneric path-element-equal (e1 e2)
  (:documentation "Compares e1 and e2.")
  (:method (e1 e2)
    nil)
  (:method ((e1 path-element-move-to-point)
            (e2 path-element-move-to-point))
    (point= (path-element-point e1)
            (path-element-point e2)))
  (:method ((e1 path-element-line-to-point)
            (e2 path-element-line-to-point))
    (point= (path-element-point e1)
            (path-element-point e2)))
  (:method ((e1 path-element-quad-curve-to-point)
            (e2 path-element-quad-curve-to-point))
    (and (point= (path-element-point           e1)  (path-element-point           e2))
         (point= (path-element-control-point   e1)  (path-element-control-point   e2))))
  (:method ((e1 path-element-curve-to-point)
            (e2 path-element-curve-to-point))
    (and (point= (path-element-point           e1)  (path-element-point           e2))
         (point= (path-element-control-point-1 e1)  (path-element-control-point-1 e2))
         (point= (path-element-control-point-2 e1)  (path-element-control-point-2 e2)))))



(defclass  path ()
  ((elements           :initarg elements           :initform '() :type list
                       :documentation "A list of path-elements, in order.")
   (last-element-cell  :initarg last-element-cell  :initform '() :type list
                       :documentation "A reference to the last cell of ELEMENTS.")
   (start-subpath-cell :initarg start-subpath-cell :initform '() :type list
                       :documentation "A reference to the first cell of the last subpath in ELEMENTS.")
   (new-subpath        :initarg new-subpath        :initform t   :type boolean
                       :documentation "Indicates whether the last subpath is closed."))
  (:documentation "A graphics path is a mathematical description of a series of shapes or lines."))


(defgeneric copy-path (path)
  (:documentation "Produces a deep copy of the PATH.  All the elements are duplicated.
Notice that the points and rect in the path-element instances are not duplicated,
since they're considered immutable."))

(defgeneric path-current-point (path)
  (:documentation "Returns the current point of the path, or NIL if it has no current point."))

(defgeneric path-empty-p              (path)
  (:documentation "Predicate whether the PATH is empty."))

(defgeneric path-rect-p               (path)
  (:documentation "If the path is a rect, returns the rect, otherwise returns NIL.
Notice: RECTs have sides parallel to the axis of the coordinate system, therefore
        very few path are rects, and some paths can be rectangular without being rects.
Notice: We don't try to check whether the path is a rect when it has
        more than four line-to.
"))

(defgeneric path-equal (path other-path)
  (:documentation "Whether the two specified paths contain the
same sequence of path elements."))

(defgeneric path-contains-point (path point eofill)
  (:documentation "A point is contained in a path if it would be
inside the painted region when the path is filled.  This depends on eofill
which may be true, in which case the Even-Odd-Fill rule is applied,
or NIL in which case the winding rule is applied."))


(defgeneric move-to-point             (path point)
  (:documentation "Starts a new subpath from the POINT."))

(defgeneric move-to-coordinates       (path x y)
  (:documentation "Starts a new subpath from the point at coordinates X and Y."))

(defgeneric line-to-point             (path point)
  (:documentation "Adds a line segment from the current point to the new POINT."))

(defgeneric line-to-coordinates       (path x y)
  (:documentation "Adds a line segment from the current point to the new point at coordinates X and Y."))

(defgeneric quad-curve-to-point       (path control-point point)
  (:documentation "Adds a quadratic Bézier curve from the current point to the new POINT using the CONTROL-POINT."))

(defgeneric quad-curve-to-coordinates (path cpx cpy x y)
  (:documentation "Adds a quadratic Bézier curve from the current point to the new point at coordinates X and Y, using the control-point at coordinates CPX and CPY."))

(defgeneric curve-to-point            (path control-point-1 control-point-2 point)
  (:documentation "Adds a Bézier curve from the current point to the new POINT using the control points CONTROL-POINT-1 and CONTROL-POINT-2."))

(defgeneric curve-to-coordinates      (path cp1x cp1y cp2x cp2y x y)
  (:documentation "Adds a Bézier curve from the current point to the new point at coordinates X and Y, using the control points at coordinates CP1X, CP1Y, and CP2X, CP2Y."))

(defgeneric close-subpath             (path)
  (:documentation "Closes the current subpath."))

(defgeneric add-rect                  (path rect)
  (:documentation "Adds the rectangle RECT as new subpath to the PATH."))

(defgeneric add-rects                 (path rects)
  (:documentation "Adds each rectangle in RECTS as new subpaths to the PATH."))

(defgeneric add-lines                 (path points)
  (:documentation "Adds the line segments from the current point to each of the POINTS in sequence."))

(defgeneric add-ellipse-in-rect       (path rect)
  (:documentation "Adds the ellipse circumscript into the RECT to the PATH."))

(defgeneric add-arc                   (path center radius start-angle end-angle clockwisep)
  (:documentation "Adds an arc of the given RADIUS and CENTER, from
the START-ANGLE to the END-ANGLE, in the clockwise or
counter-clockwise direction."))

(defgeneric add-arc-to-point          (path start-point end-point radius)
  (:documentation "
The current point and start-point define a line, and the start-point
and the end-point define another line.

This function adds a set of Bézier curves to draw an arc of the given
RADIUS that is tangent to those two lines.   The start and end of the
arc are the tangent points.  If the start is not the current point,
then a line is added before the arc.

Note: if the current point, start-point and end-point are colinear,
then it merely adds two line segments."))

(defgeneric add-path                  (path other-path)
  (:documentation "Appends the elements from OTHER-PATH to the PATH."))

(defgeneric surrounding-box              (path)
  (:documentation "Returns the surrounding box of the specified path.  If
the path is empty, this function returns the null rect.  The
surrounding box is the smallest rectangle completely enclosing all points
in the path, including control points for Bézier and quadratic
curves."))

(defgeneric bounding-box              (path)
  (:documentation "Returns the path bounding box of the specified
path. If the path is empty, this function returns the null rect.  The
path bounding box is the smallest rectangle completely enclosing all
points in the path but not including control points for Bézier and
quadratic curves."))


(defgeneric path-apply (path examiner-closure)
  (:documentation "For each element in the specified path, 
calls the examiner function, which can examine (but not modify) the
element."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *path-class* 'path)

(defun create-path ()
  (make-instance *path-class*))

(defun position-of-cell (list cell)
  "Returns the position of the CELL in the LIST"
  (loop
     :for current :on list
     :for position :from 0
     :when (eq current cell)
     :do (return position)
     :finally (return nil)))

(defun test/position-of-cell ()
  (let ((list (list 1 2 3 4 5 6)))
    (assert (null (position-of-cell list (cons nil nil))))
    (assert (equal  (maplist (lambda (cell) (position-of-cell list cell)) list)
                    '(0 1 2 3 4 5)))
    :success))



(defparameter *point-zero* (point 0 0))
(defparameter *null-rect*  (rect 0 0 0 0))


(defmethod copy-path ((path path))
  (with-slots (elements start-subpath-cell new-subpath) path
    (let ((new-elements (mapcar (function copy-path-element) elements)))
     (make-instance 'path
       'elements           new-elements
       'start-subpath-cell (nthcdr (position-of-cell elements start-subpath-cell) new-elements)
       'last-element-cell  (last new-elements)
       'new-subpath        new-subpath))))


(defmethod path-current-point ((path path))
  (with-slots (start-subpath-cell last-element-cell new-subpath) path
    (cond
      ((null last-element-cell)         ; empty path
       *point-zero*)
      (new-subpath
       (path-element-point (first start-subpath-cell)))
      (t
       (path-element-point (first last-element-cell))))))


(defmethod path-empty-p              (path)
  (with-slots (last-element-cell) path
    (null last-element-cell)))


(defmethod path-rect-p (path)
  (with-slots (elements) path
   (when (and (= 5 (length))
              (typep (first elements) 'path-element-move-to-point)
              (every (lambda (element) (typep element 'path-element-line-to-point))
                     (rest elements)))
     (let ((p0 (path-element-point (first  elements)))
           (p1 (path-element-point (second elements)))
           (p2 (path-element-point (third  elements)))
           (p3 (path-element-point (fourth elements)))
           (p4 (path-element-point (fifth  elements))))
       (when (and (point= p0 p4)
                  (or (and (= (point-x p0) (point-x p1))
                           (= (point-y p1) (point-y p2))
                           (= (point-x p2) (point-x p3))
                           (= (point-y p3) (point-y p4)))
                      (and (= (point-y p0) (point-y p1))
                           (= (point-x p1) (point-x p2))
                           (= (point-y p2) (point-y p3))
                           (= (point-x p3) (point-x p4)))))
         (let ((left   (min (point-x p0) (point-x p1) (point-x p2)  (point-x p3)))
               (bottom (min (point-y p0) (point-y p1) (point-y p2)  (point-y p3)))
               (right  (max (point-x p0) (point-x p1) (point-x p2)  (point-x p3)))
               (top    (max (point-y p0) (point-y p1) (point-y p2)  (point-y p3))))
          (make-rect left bottom (- right left) (- top bottom))))))))


(defmethod path-equal (path other-path)
  (or (eql path other-path)
      (with-slots ((elements e1)) path
        (with-slots ((elements e2)) other-path
          (loop
             :for ce1 = e1 :then (cdr ce1)
             :for ce2 = e2 :then (cdr ce2)
             :until (and (null ce1) (null ce2))
             :always (and ce1 ce2  (path-element-equal (car ce1) (car ce2))))))))



(defmethod path-contains-point ((path path) point (eofill null))
  (not-implemented-yet))


(defun add-element (path element)
  "Add the ELEMENT to the PATH.  If the last subpath is closed
\(new-subpath is true), then ensure that the new subpath starts with a
move-to-point path element."
  (with-slots (elements start-subpath-cell last-element-cell new-subpath) path
    (let ((new-element (list element)))
      (if last-element-cell
          (progn
            ;; If the last subpath is closed (it's a new-subpath),
            ;; and the new element is not a move-to-point,
            ;; then add a move-to-point to the previous start point.
            (when new-subpath
              (typecase element
                (path-element-move-to-point
                 (setf start-subpath-cell new-element
                       new-subpath nil))
                (t
                 (add-element path (make-instance 'path-element-move-to-point
                                       :point (path-element-point (first start-subpath-cell)))))))
            (setf (cdr last-element-cell) new-element
                  last-element-cell new-element))
          (progn
            (typecase element
              (path-element-move-to-point
               (setf elements new-element
                     last-element-cell new-element
                     start-subpath-cell new-element
                     new-subpath nil))
              (t
               (add-element path (make-instance 'path-element-move-to-point
                                     :point *point-zero*))
               (setf (cdr last-element-cell) new-element
                     last-element-cell new-element)))))))
  (values))



(defmethod move-to-point             ((path path)  point)
  (add-element path (make-instance 'path-element-move-to-point :point point)))

(defmethod move-to-coordinates       (path x y)
  (move-to-point path (point x y)))


(defmethod line-to-point             ((path path) point)
  (add-element path (make-instance 'path-element-line-to-point :point point)))

(defmethod line-to-coordinates       (path x y)
  (line-to-point path (point x y)))



(defmethod quad-curve-to-point       ((path path) (control-point point) point)
  (add-element path (make-instance 'path-element-quad-curve-to-point
                      :point point
                      :control-point control-point)))


(defmethod quad-curve-to-coordinates (path cpx cpy x y)
  (quad-curve-to-point path (point cpx cpy) (point x y)))


(defmethod curve-to-point            ((path path) control-point-1 control-point-2 point)
  (add-element path (make-instance 'path-element-curve-to-point
                      :point point
                      :control-point-1 control-point-1
                      :control-point-2 control-point-2)))

(defmethod curve-to-coordinates      (path cp1x cp1y cp2x cp2y x y)
  (curve-to-point path (point cp1x cp1y) (point cp2x cp2y) (point x y)))


(defmethod close-subpath             ((path path))
  (with-slots (new-subpath) path
    (setf new-subpath t))
  (values))


(defmethod add-rect                  (path rect)
  (move-to-point path (rect-origin rect)))


(defmethod add-rects                 (path rects)
  (map nil (lambda (rect) (add-rect path rect)) rects))


(defmethod add-lines                 (path points)
  (let ((first-time t))
    (map nil (lambda (point)
               (if first-time
                   (progn (setf first-time nil)
                          (move-to-point path point))
                   (line-to-point path point)))
         points)))


(defmethod add-ellipse-in-rect       (path rect)
  (add-path path (elliptical-arc (rect-center-x rect) (rect-center-y rect)
                                 (/ (rect-width rect) 2) (/ (rect-height rect 2)))))

(defmethod add-arc                   (path center radius start-angle end-angle clockwisep)
  (let ((ellipse (elliptical-arc (point-x center) (point-y center)
                                 radius radius
                                 0
                                 (if clockwisep end-angle start-angle)
                                 (if clockwisep start-angle end-angle))))
    (add-path path ellipse)))


(defmethod add-arc-to-point          (path start-point end-point radius)
  (let* ((current-point (path-current-point path))
         (v1            (vector- start-point current-point))
         (v2            (vector- end-point   start-point)))
    (if (zerop (dot-product v1 v2))
        (progn
          (line-to-point path start-point)
          (line-to-point path end-point))
        (let* ((ccw           (plusp (determinant v1 v2)))
               (perpendicular (if ccw
                                  pi/2
                                  (- pi/2)))
               (r1     (vector* radius (rotate perpendicular (unit-vector v1))))
               (r2     (vector* radius (rotate perpendicular (unit-vector v2))))
               (center (line-intersection (vector+ current-point r1) v1
                                          (vector+ start-point   r2) v2))
               (start-angle (vector-angle (size 1 0) r1))
               (end-angle   (vector-angle r1 r2))
               (start-point (vector- center r1)))
          (unless (point= start-point current-point)
            (line-to-point path start-point))
          (add-path path (elliptical-arc (point-x center) (point-y center) radius radius
                                    0 start-angle end-angle))))))


(defmethod add-path                  (path other-path)
  (path-apply other-path (lambda (element) (add-element path element))))


(defgeneric expand-surrounding-box (element rect)
  (:method ((element path-element) rect)
    rect)
  (:method ((element path-element-move-to-point) rect)
    (rect-expand rect (path-element-point element)))
  (:method ((element path-element-line-to-point) rect)
    (rect-expand rect (path-element-point element)))
  (:method ((element path-element-quad-curve-to-point) rect)
    (rect-expand rect (path-element-point element))
    (rect-expand rect (path-element-control-point element)))
  (:method ((element path-element-curve-to-point) rect)
    (rect-expand rect (path-element-point element))
    (rect-expand rect (path-element-control-point-1 element))
    (rect-expand rect (path-element-control-point-2 element))))

(defmethod surrounding-box              (path)
  (if (path-empty-p path)
    *null-rect*
    (let ((surrounding (rect (path-element-point (first (slot-value path 'elements))) (size 0 0))))
      (path-apply path (lambda (element) (expand-surrounding-box element surrounding)))
      surrounding)))

(defmethod bounding-box              (path)
  (not-implemented-yet))

(defmethod path-apply (path examiner-closure)
  (dolist (element (slot-value path 'elements) (values))
    (funcall examiner-closure element)))



;;;; THE END ;;;;
