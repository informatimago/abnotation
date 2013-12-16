;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               bezier.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Maps the bezier PATH lisp class to NSBezier Objective-C class.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-12-16 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2013 - 2013
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
(in-package "ABNOTATION.COCOA")
(objcl:set-objective-cl-syntax)

(defclass ns-bezier-path (path)
  ((bezier)
   (dirty  :initform t :reader dirty)))


(defmethod compute-bezier-path ((path ns-bezier-path))
  (let ((bezier [NSBezierPath bezierPath]))
    (path-apply path
                (lambda (element)
                  (etypecase element
                    (path-element-move-to-point
                     [bezier moveToPoint:(nspoint (path-element-point element))])
                    (path-element-line-to-point
                     [bezier lineToPoint:(nspoint (path-element-point element))])
                    (path-element-quad-curve-to-point
                     (let ((2/3v (vector* 2/3 (path-element-control-point element))))
                       [bezier curveToPoint:(nspoint (path-element-point element))
                               controlPoint1:(nspoint (vector+ 2/3v (vector* 1/3 (get-nspoint [bezier currentPoint]))))
                               controlPoint2:(nspoint (vector+ 2/3v (vector* 1/3 (path-element-point element))))]))
                    (path-element-curve-to-point
                     [bezier curveToPoint:(nspoint (path-element-point element))
                             controlPoint1:(nspoint (path-element-control-point-1 element))
                             controlPoint2:(nspoint (path-element-control-point-2 element))]))))
    bezier))


(defmethod bezier-path ((path ns-bezier-path))
  (if (slot-value path 'dirty)
      (setf (slot-value path 'dirty)  nil
            (slot-value path 'bezier) (compute-bezier-path path))
      (slot-value path 'bezier)))

(defmethod move-to-point ((path ns-bezier-path) point) 
  (setf (slot-value path (quote dirty)) t) 
  (call-next-method))
(defmethod move-to-coordinates ((path ns-bezier-path) x y) 
  (setf (slot-value path (quote dirty)) t) 
  (call-next-method))
(defmethod line-to-point ((path ns-bezier-path) point) 
  (setf (slot-value path (quote dirty)) t) 
  (call-next-method))
(defmethod line-to-coordinates ((path ns-bezier-path) x y) 
  (setf (slot-value path (quote dirty)) t) 
  (call-next-method))
(defmethod quad-curve-to-point ((path ns-bezier-path) control-point point) 
  (setf (slot-value path (quote dirty)) t) 
  (call-next-method))
(defmethod quad-curve-to-coordinates ((path ns-bezier-path) cpx cpy x y) 
  (setf (slot-value path (quote dirty)) t) 
  (call-next-method))
(defmethod curve-to-point ((path ns-bezier-path) control-point-1 control-point-2 point) 
  (setf (slot-value path (quote dirty)) t) 
  (call-next-method))
(defmethod curve-to-coordinates ((path ns-bezier-path) cp1x cp1y cp2x cp2y x y) 
  (setf (slot-value path (quote dirty)) t) 
  (call-next-method))
(defmethod close-subpath ((path ns-bezier-path)) 
  (setf (slot-value path (quote dirty)) t) 
  (call-next-method))
(defmethod add-rect ((path ns-bezier-path) rect) 
  (setf (slot-value path (quote dirty)) t) 
  (call-next-method))
(defmethod add-rects ((path ns-bezier-path) rects) 
  (setf (slot-value path (quote dirty)) t) 
  (call-next-method))
(defmethod add-lines ((path ns-bezier-path) points) 
  (setf (slot-value path (quote dirty)) t) 
  (call-next-method))
(defmethod add-ellipse-in-rect ((path ns-bezier-path) rect) 
  (setf (slot-value path (quote dirty)) t) 
  (call-next-method))
(defmethod add-arc ((path ns-bezier-path) center radius start-angle end-angle clockwisep) 
  (setf (slot-value path (quote dirty)) t) 
  (call-next-method))
(defmethod add-arc-to-point ((path ns-bezier-path) start-point end-point radius) 
  (setf (slot-value path (quote dirty)) t) 
  (call-next-method))
(defmethod add-path ((path ns-bezier-path) other-path) 
  (setf (slot-value path (quote dirty)) t) 
  (call-next-method))

;;;; THE END ;;;;