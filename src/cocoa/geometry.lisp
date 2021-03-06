;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               abgeometry.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Geometry: converting coordinates, points, and rectangle between
;;;;    lisp and Cocoa.
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

(in-package "ABNOTATION.COCOA")
(objcl:enable-objcl-reader-macros)


;;; Cocoa uses the normal mathematical coordinates system, with the
;;; origin on the bottom-left corner, and Y axis going upward.
;;;
;;; We use the same coordinate systems, with floating-point numbers,
;;; in units of millimeter.


(defmethod place (object (to ns:ns-point))
    (setf (origin object) to)
    object)

;;;------------------------------------------------------------
;;; Conversions between ns:ns-point, ns:ns-size, ns:ns-rect and
;;; point, size and rect.

(defmethod to-lisp ((nspoint ns:ns-point))
  (make-point :x     (ns:ns-point-x nspoint)
              :y     (ns:ns-point-y nspoint)))

(defmethod to-lisp ((nssize ns:ns-size))
  (make-size :width   (ns:ns-size-width nssize)
             :height  (ns:ns-size-height nssize)))

(defmethod to-lisp ((rect ns:ns-rect))
  (make-rect :x (rect-x rect) :y (rect-y rect)
             :width (rect-width rect)
             :height (rect-height rect)))

(defmethod to-lisp ((range ns:ns-range))
  (make-range :location (range-location range) :length (range-length range)))

(defmethod to-objc ((point point))
  (ns:make-ns-point (point-x point) (point-y point)))

(defmethod to-objc ((size size))
  (ns:make-ns-size (size-width size) (size-height size)))

(defmethod to-objc ((rect rect))
  (ns:make-ns-rect (rect-x rect) (rect-y rect)
                   (rect-width rect) (rect-height rect)))

(defmethod to-objc ((range range))
  (ns:make-ns-range (range-location range) (range-length range)))

(defmethod to-objc ((string string))
  (objcl:objcl-string string))


(defun rect-from-macptr (ptr)
  (ecase (type-of (coerce 0.0 'coordinate))
    (single-float (rect (ccl::%get-single-float ptr 0)
                        (ccl::%get-single-float ptr 4)
                        (ccl::%get-single-float ptr 8)
                        (ccl::%get-single-float ptr 12)))
    (double-float (rect (ccl::%get-double-float ptr 0)
                        (ccl::%get-double-float ptr 8)
                        (ccl::%get-double-float ptr 16)
                        (ccl::%get-double-float ptr 24)))))



(defmethod wrap ((nspoint ns:ns-point))
  (wrapping nspoint
            (make-point :x     (ns:ns-point-x nspoint)
                        :y     (ns:ns-point-y nspoint))))

(defmethod wrap ((nssize ns:ns-size))
  (wrapping nssize
            (make-size :width  (ns:ns-size-width nssize)
                       :height (ns:ns-size-height nssize))))

(defmethod wrap ((nsrect ns:ns-rect))
  (wrapping nsrect
            (make-rect :x      (ns:ns-rect-x nsrect)
                       :y      (ns:ns-rect-y nsrect)
                       :width  (ns:ns-rect-width nsrect)
                       :height (ns:ns-rect-height nsrect))))

(defmethod wrap ((nsrange ns:ns-range))
  (wrapping nsrange
            (make-range :location (ns:ns-range-location nsrange)
                        :length   (ns:ns-range-length   nsrange))))


(defmethod unwrap ((point point))
  (unwrapping point
              (ns:make-ns-point (point-x point) (point-y point))))

(defmethod unwrap ((size size))
  (unwrapping size
              (ns:make-ns-size (size-width size) (size-height size))))

(defmethod unwrap ((rect rect))
  (unwrapping rect
              (ns:make-ns-rect (rect-x rect) (rect-y rect)
                               (rect-width rect) (rect-height rect))))


(defmethod unwrap ((range range))
  (unwrapping range
              (ns:make-ns-range (range-location range) (range-length range))))



;; Shortcuts:

(defun nsrect (pos siz)
  (ns:make-ns-rect (point-x pos) (point-y pos) (point-x siz) (point-y siz)))

(defun nspoint (pos)
  (ns:make-ns-point (point-x pos) (point-y pos)))

(defun nssize (siz)
  (ns:make-ns-size (point-x siz) (point-y siz)))

(defun nsrect-to-list (nsrect)
  (list (ns:ns-rect-x nsrect)
        (ns:ns-rect-y nsrect)
        (ns:ns-rect-width nsrect)
        (ns:ns-rect-height nsrect)))

(declaim (inline nsrect-to-list nsrect nspoint nssize))


(defmacro get-nspoint (call)
  (let ((vpoint (gensym)))
    `(oclo:slet ((,vpoint ,call)) (wrap ,vpoint))))

(defmacro get-nssize (call)
  (let ((vsize (gensym)))
    `(oclo:slet ((,vsize ,call)) (wrap ,vsize))))

(defmacro get-nsrect (call)
  (let ((vrect (gensym)))
    `(oclo:slet ((,vrect ,call)) (wrap ,vrect))))



;; layout stuff:

(defmethod width  ((r ns:ns-size)) (ns:ns-size-width r))
(defmethod height ((r ns:ns-size)) (ns:ns-size-height r))
(defmethod (setf width)  (new-value (r ns:ns-size)) (setf (ns:ns-size-width  r) new-value))
(defmethod (setf height) (new-value (r ns:ns-size)) (setf (ns:ns-size-height r) new-value))

;; (symbol-package 'defmethod)#<Package "OBJC">
;; (macroexpand-1 '(defmethod (setf width)  (new-value (r ns:ns-size)) (setf (ns:ns-size-width  r) new-value)))


(defmethod left   ((r ns:ns-rect)) (ns:ns-rect-x      r))
(defmethod bottom ((r ns:ns-rect)) (ns:ns-rect-y      r))
(defmethod width  ((r ns:ns-rect)) (ns:ns-rect-width  r))
(defmethod height ((r ns:ns-rect)) (ns:ns-rect-height r))
(defmethod (setf left)   (new-value (r ns:ns-rect)) (setf (ns:ns-rect-x      r) new-value))
(defmethod (setf bottom) (new-value (r ns:ns-rect)) (setf (ns:ns-rect-y      r) new-value))
(defmethod (setf width)  (new-value (r ns:ns-rect)) (setf (ns:ns-rect-width  r) new-value))
(defmethod (setf height) (new-value (r ns:ns-rect)) (setf (ns:ns-rect-height r) new-value))



(defmethod origin ((r ns:ns-rect))
  (rect-origin r))

(defmethod (setf origin) (new-value (r ns:ns-rect))
  (setf (rect-origin r) new-value))

(defmethod frame ((r ns:ns-rect))
  (wrap r))


(defmethod bounds ((r ns:ns-rect))
  (make-rect :size (rect-size r)))

(defmethod place ((r ns:ns-rect) (to ns:ns-point))
  (setf (rect-origin r) to)
  r)



(defmethod above ((r ns:ns-point) &optional (offset 0))
  (make-point :x (point-x r) :y (+ (point-y r) offset)))

(defmethod below ((r ns:ns-point) &optional (offset 0))
  (make-point :x (point-x r) :y (- (point-y r) offset)))

(defmethod left-of ((r ns:ns-point) &optional (offset 0))
  (make-point :x (- (point-x r) offset) :y (point-y r)))

(defmethod right-of ((r ns:ns-point) &optional (offset 0))
  (make-point :x (+ (point-x r) offset) :y (point-y r)))


;;;; THE END ;;;;
