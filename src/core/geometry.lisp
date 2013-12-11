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


(deftype coordinate ()
  (or (and (find-package "NS") (find-symbol "CGFLOAT" "NS"))  ;; can be 32-bit or 64-bit float.
      'single-float))
(setf *read-default-float-format* 'coordinate)
(defun coordinate (value) (coerce value 'coordinate))
(declaim (inline coordinate))


(defstruct (point (:constructor %make-point))
  (x      (coordinate 0.0d0) :type coordinate)
  (y      (coordinate 0.0d0) :type coordinate))

(defun make-point (&key (x 0.0d0) (y 0.0d0))
  (%make-point :x (coordinate x) :y (coordinate y)))


(defstruct (size (:constructor %make-size))
  (width  (coordinate 0.0d0) :type coordinate)
  (height (coordinate 0.0d0) :type coordinate))

(defun make-size (&key (width 0.0d0) (height 0.0d0))
  (%make-size :width (coordinate width) :height (coordinate height)))


(defstruct (rect (:constructor %make-rect))
  (x      (coordinate 0.0d0) :type coordinate)
  (y      (coordinate 0.0d0) :type coordinate)
  (width  (coordinate 0.0d0) :type coordinate)
  (height (coordinate 0.0d0) :type coordinate))

(defun rect-left   (r) (rect-x r))
(defun rect-right  (r) (+ (rect-x r) (rect-width r)))
(defun rect-bottom (r) (rect-y r))
(defun rect-top    (r) (+ (rect-x r) (rect-height r)))
(declaim (inline rect-left rect-right rect-bottom rect-top))

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


(defstruct (range (:constructor %make-range))
  (location 0 :type unsigned-byte)
  (length   0 :type unsigned-byte))

(defun make-range (&key (location 0) (length 0))
  (%make-range :location (truncate location) :length (truncate length)))



;;;; THE END ;;;;
