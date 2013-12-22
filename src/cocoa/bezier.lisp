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

(defclass cocoa-bezier-path (path)
  ((bezier)
   (dirty  :initform t :reader dirty)))


(defmethod compute-bezier-path ((path cocoa-bezier-path))
  (let ((bezier [NSBezierPath bezierPath]))
    (path-apply path
                (lambda (element)
                  (etypecase element
                    (path-element-move-to-point
                     [bezier moveToPoint:(nspoint (path-element-point element))])
                    (path-element-line-to-point
                     [bezier lineToPoint:(nspoint (path-element-point element))])
                    (path-element-quad-curve-to-point
                     (let ((2/3v (vector* #.(coerce 2/3 'coordinate) (path-element-control-point element))))
                       [bezier curveToPoint:(nspoint (path-element-point element))
                               controlPoint1:(nspoint (vector+ 2/3v (vector* #.(coerce 1/3 'coordinate) (get-nspoint [bezier currentPoint]))))
                               controlPoint2:(nspoint (vector+ 2/3v (vector* #.(coerce 1/3 'coordinate) (path-element-point element))))]))
                    (path-element-curve-to-point
                     [bezier curveToPoint:(nspoint (path-element-point element))
                             controlPoint1:(nspoint (path-element-control-point-1 element))
                             controlPoint2:(nspoint (path-element-control-point-2 element))]))))
    bezier))


(defmethod bezier-path ((path cocoa-bezier-path))
  (if (slot-value path 'dirty)
      (prog1 (setf (slot-value path 'bezier) (compute-bezier-path path))
        (setf (slot-value path 'dirty)  nil))
      (slot-value path 'bezier)))

(defmethod move-to-point ((path cocoa-bezier-path) transform point) 
  (setf (slot-value path (quote dirty)) t) 
  (call-next-method))
(defmethod move-to-coordinates ((path cocoa-bezier-path) transform x y) 
  (setf (slot-value path (quote dirty)) t) 
  (call-next-method))
(defmethod line-to-point ((path cocoa-bezier-path) transform point) 
  (setf (slot-value path (quote dirty)) t) 
  (call-next-method))
(defmethod line-to-coordinates ((path cocoa-bezier-path) transform x y) 
  (setf (slot-value path (quote dirty)) t) 
  (call-next-method))
(defmethod quad-curve-to-point ((path cocoa-bezier-path) transform control-point point) 
  (setf (slot-value path (quote dirty)) t) 
  (call-next-method))
(defmethod quad-curve-to-coordinates ((path cocoa-bezier-path) transform cpx cpy x y) 
  (setf (slot-value path (quote dirty)) t) 
  (call-next-method))
(defmethod curve-to-point ((path cocoa-bezier-path) transform control-point-1 control-point-2 point) 
  (setf (slot-value path (quote dirty)) t) 
  (call-next-method))
(defmethod curve-to-coordinates ((path cocoa-bezier-path) transform cp1x cp1y cp2x cp2y x y) 
  (setf (slot-value path (quote dirty)) t) 
  (call-next-method))
(defmethod close-subpath ((path cocoa-bezier-path)) 
  (setf (slot-value path (quote dirty)) t) 
  (call-next-method))
(defmethod add-rect ((path cocoa-bezier-path) transform rect) 
  (setf (slot-value path (quote dirty)) t) 
  (call-next-method))
(defmethod add-rects ((path cocoa-bezier-path) transform rects) 
  (setf (slot-value path (quote dirty)) t) 
  (call-next-method))
(defmethod add-lines ((path cocoa-bezier-path) transform points) 
  (setf (slot-value path (quote dirty)) t) 
  (call-next-method))
(defmethod add-ellipse-in-rect ((path cocoa-bezier-path) transform rect) 
  (setf (slot-value path (quote dirty)) t) 
  (call-next-method))
(defmethod add-arc ((path cocoa-bezier-path) transform center radius start-angle end-angle clockwisep) 
  (setf (slot-value path (quote dirty)) t) 
  (call-next-method))
(defmethod add-arc-to-point ((path cocoa-bezier-path) transform start-point end-point radius) 
  (setf (slot-value path (quote dirty)) t) 
  (call-next-method))
(defmethod add-path ((path cocoa-bezier-path) transform other-path) 
  (setf (slot-value path (quote dirty)) t) 
  (call-next-method))

;;;; THE END ;;;;
