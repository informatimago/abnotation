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

;;;------------------------------------------------------------
;;; Conversions between ns:ns-point, ns:ns-size, ns:ns-rect and
;;; point, size and rect.

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

(defun size-width  (p) (ns:ns-size-width p))
(defun size-height (p) (ns:ns-size-height p))
(defun (setf size-width)  (new-value p) (setf (ns:ns-size-width  p) new-value))
(defun (setf size-height) (new-value p) (setf (ns:ns-size-height p) new-value))


(defun rect-x      (p) (ns:ns-rect-x      p))
(defun rect-y      (p) (ns:ns-rect-y      p))
(defun rect-width  (p) (ns:ns-rect-width  p))
(defun rect-height (p) (ns:ns-rect-height p))
(defun (setf rect-x)      (new-value p) (setf (ns:ns-rect-x      p) new-value))
(defun (setf rect-y)      (new-value p) (setf (ns:ns-rect-y      p) new-value))
(defun (setf rect-width)  (new-value p) (setf (ns:ns-rect-width  p) new-value))
(defun (setf rect-height) (new-value p) (setf (ns:ns-rect-height p) new-value))



(defmethod origin ((self ns:ns-rect))
  (rect-origin self))

(defmethod (setf origin) (new-value (self ns:ns-rect))
  (setf (rect-origin self) new-value))

(defmethod frame ((self ns:ns-rect))
  (wrap self))


(defmethod bounds ((self ns:ns-rect))
  (make-rect :size (rect-size self)))

(defmethod place ((self ns:ns-rect) (to ns:ns-point))
  (setf (rect-origin self) to)
  self)



(defmethod above ((self ns:ns-point) &optional (offset 0))
  (make-point :x (point-x self) :y (+ (point-y self) offset)))

(defmethod below ((self ns:ns-point) &optional (offset 0))
  (make-point :x (point-x self) :y (- (point-y self) offset)))

(defmethod left-of ((self ns:ns-point) &optional (offset 0))
  (make-point :x (- (point-x self) offset) :y (point-y self)))

(defmethod right-of ((self ns:ns-point) &optional (offset 0))
  (make-point :x (+ (point-x self) offset) :y (point-y self)))


;;;; THE END ;;;;
