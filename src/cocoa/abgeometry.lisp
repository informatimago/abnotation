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


;;;; THE END ;;;;
