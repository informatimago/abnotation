;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               draw.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Draw a partition.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-12-23 <PJB> Created.
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
(in-package "ABNOTATION.CORE")

(defvar *transform* +identity-transformation+)

(defmacro with-bounds-and-clip-rect ((bounds clip-rect) &body body)
  (let ((vbounds (gensym)))
    `(let* ((,vbounds ,bounds)
            (*transform* (matrix-translate *transform* (rect-x ,vbounds) (rect-y ,vbounds)))
            (,clip-rect  (rect-offset ,clip-rect (- (rect-x ,vbounds)) (- (rect-y ,vbounds)))))
       (declare (ignorable ,clip-rect))
       ,@body)))


(defun draw-point (x0 y0 width)
  (let ((p (create-path)))
    (move-to-coordinates p *transform* (- x0 width) (- y0 width))
    (line-to-coordinates p *transform* (+ x0 width) (- y0 width))
    (line-to-coordinates p *transform* (+ x0 width) (+ y0 width))
    (line-to-coordinates p *transform* (- x0 width) (+ y0 width))
    (line-to-coordinates p *transform* (- x0 width) (- y0 width))
    (close-subpath p)
    (fill-path p)))

(defun draw-line (x0 y0 x1 y1 width)
  (let ((p (create-path)))
    (move-to-coordinates p *transform* x0 y0)
    (line-to-coordinates p *transform* x1 y1)
    (set-stroke-width p width)
    (stroke-path p)))


(defvar *thick* 0.1 #|mm|#)
(defmethod draw ((r rect) &optional clip-rect)
  (declare (ignore clip-rect))
  (let* ((p      (create-path))
         (left   (rect-left   r))
         (right  (rect-right  r))
         (top    (rect-top    r))
         (bottom (rect-bottom r))
         (thick  *thick*)) 
    (move-to-coordinates p *transform* left bottom)
    (line-to-coordinates p *transform* left top)
    (line-to-coordinates p *transform* right top)
    (line-to-coordinates p *transform* right bottom)
    (line-to-coordinates p *transform* left bottom)
    (move-to-coordinates p *transform* (+ left thick) (+ bottom thick))
    (line-to-coordinates p *transform* (- right thick) (+ bottom thick))
    (line-to-coordinates p *transform* (- right thick) (- top thick))
    (line-to-coordinates p *transform* (+ left thick) (- top thick))
    (line-to-coordinates p *transform* (+ left thick) (+ bottom thick))
    (close-subpath p)
    (fill-path p)))


(defmethod fill-rect ((r rect) &optional clip-rect)
  (declare (ignore clip-rect))
  (let* ((p      (create-path))
         (left   (rect-left   r))
         (right  (rect-right  r))
         (top    (rect-top    r))
         (bottom (rect-bottom r))) 
    (move-to-coordinates p *transform* left bottom)
    (line-to-coordinates p *transform* left top)
    (line-to-coordinates p *transform* right top)
    (line-to-coordinates p *transform* right bottom)
    (line-to-coordinates p *transform* left bottom)
    (close-subpath p)
    (fill-path p)))



(defgeneric debug-color (element)
  (:method ((element t))       :light-gray)
  (:method ((element page))    :dark-gray)
  (:method ((element staff))   :yellow)
  (:method ((element clef))    :green)
  (:method ((element line))    :blue)
  (:method ((element measure)) :cyan)
  (:method ((element sound))   :orange))

(defun box-and-absolute (box)
  (list :rel box
        :abs (transform-rect *transform*
                             (rect-x box) (rect-y box) (rect-width box) (rect-height box))
        :flp [[NSGraphicsContext currentContext] isFlipped]
        :att [[NSGraphicsContext currentContext] attributes]
        :tra *transform*))

(defmethod draw :before ((element graphic-element) &optional clip-rect)
  (set-color (debug-color element))
  (if (eq :yellow (debug-color element))
      (let ((*thick* 0.5))
        (format *trace-output* "Yellow box: ~S~%" (box-and-absolute (box element)))
        (draw (box element) clip-rect))
      (draw (box element) clip-rect)))

(defmethod draw :after ((element graphic-element) &optional clip-rect)
  (when (annotation element)
    (draw (annotation element) clip-rect)))


(defmethod draw ((element image) &optional clip-rect)
  (format *trace-output* "Drawing image ~S ~S~%"
          (debug-color element) (box-and-absolute (box element)))
  (with-bounds-and-clip-rect ((box element) clip-rect)
    ;; TODO: draw image in (box element)
    ))

(defmethod draw ((element text) &optional clip-rect)
  (format *trace-output* "Drawing text ~S ~S~%"
          (debug-color element) (box-and-absolute (box element)))
  (with-bounds-and-clip-rect ((box element) clip-rect)
    ;; TODO: draw rich text in (box element)
    ))

(defconstant +degree+ (/ pi 180))
(defmethod draw ((element tete) &optional clip-rect)
  (declare (ignore clip-rect))
  (let ((box (box element)))
    (format *trace-output* "Drawing tete ~S ~S~%"
            (debug-color element) (box-and-absolute (box element)))
    (fill-path (elliptical-arc (rect-center-x box) (rect-center-y box)
                               (width box) (height box)
                               (* 10 +degree+) 0.0d0 2pi 2 1.0d-6 *transform*))))


(defmethod draw ((element accidental) &optional clip-rect)
  (format *trace-output* "Drawing accidental ~S ~S~%"
          (debug-color element) (box-and-absolute (box element)))
  (with-bounds-and-clip-rect ((box element) clip-rect)
    (set-color :black)
    #| TODO: draw ♮♯♭ in box |#))

(defmethod draw ((element tenue-segment) &optional clip-rect)
  (format *trace-output* "Drawing tenue-segment ~S ~S~%"
          (debug-color element) (box-and-absolute (box element)))
  (with-bounds-and-clip-rect ((box element) clip-rect)
    (set-color :black)
    #| TODO: draw tenue segment in box |#))

(defmethod draw ((element dynamic-segment) &optional clip-rect)
  (format *trace-output* "Drawing dynamic-segment ~S ~S~%"
          (debug-color element) (box-and-absolute (box element)))
  (with-bounds-and-clip-rect ((box element) clip-rect)
    (set-color :black)
    #| TODO: draw dynamic segment in box |#))

(defmethod draw ((element beam-segment) &optional clip-rect)
  (format *trace-output* "Drawing beam-segment ~S ~S~%"
          (debug-color element) (box-and-absolute (box element)))
  (with-bounds-and-clip-rect ((box element) clip-rect)
    (set-color :black)
    #| TODO: draw beam segment in box |#))


(defmethod on-line-p ((note note))
  ;; TODO: (f (pitch note))
  nil)

(defmethod draw ((ledger ledger) &optional clip-rect)
  (format *trace-output* "Drawing ledger ~A-~A ~S ~S~%"
          (minimum-lane ledger) (maximum-lane ledger)
          (debug-color ledger) (box ledger))
  (with-bounds-and-clip-rect ((box ledger) clip-rect)
    (set-color :black)
    (dolist (note (notes ledger))
      (when (on-line-p note)
        ;; (draw-ledger-line (rect-inset (box (tete note)) -1.0 0))
        ))))


(defmethod draw ((clef clef) &optional clip-rect)
  (format *trace-output* "Drawing clef ~A ~S ~S~%" (name clef)
          (debug-color clef) (box-and-absolute (box clef)))
  (let* ((partition (partition (page (line (staff clef)))))
         (height    (* (ecase (trait clef) ;; TODO: clean this horrible hack.
                         (4 -3)
                         (2 -7))
                       (lane-height partition)))
         (font-size (* 0.75 (height (box clef)))))
    (with-bounds-and-clip-rect ((box clef) clip-rect)
      (let ((where (transform-point *transform* 0 height))) 
        (draw-clef (name clef) where font-size) 
        (set-color :green)
        (draw-point 0 0 0.5)))))

(defmethod draw ((staff staff) &optional clip-rect)
  (format *trace-output* "Drawing staff ~A ~A-~A ~S ~S~%"
          (name (clef staff)) (minimum-lane staff) (maximum-lane staff)
          (debug-color staff) (box-and-absolute (box staff)))
  (unless (rect-empty-p (rect-intersection clip-rect
                                           (let ((r (box staff)))
                                             (rect 0 0 (rect-width r) (rect-height r)))))
    (with-bounds-and-clip-rect ((box staff) clip-rect)
      (set-color :black)
      (loop
        :with staff-height = (staff-height (partition (page (line staff))))
        :with left  = (left  (box staff))
        :with right = (right (box staff))
        :for y :from 0 :to staff-height :by (/ staff-height 4)
        :do (draw-line left y right y (/ staff-height 45.0)))
      (draw (clef staff) clip-rect))))



(defmethod draw ((measure measure) &optional clip-rect)
  (format *trace-output* "Drawing measure ~A ~S ~S~%" (number measure)
          (debug-color measure) (box-and-absolute (box measure)))
  (with-bounds-and-clip-rect ((box measure) clip-rect)
    ;; TODO: (draw-measure-hat measure clip-rect)
    ;; TODO: (draw-measure-bar measure clip-rect)
    (draw (number-annotation measure) clip-rect)))

(defmethod draw ((line line) &optional clip-rect)
  (format *trace-output* "Drawing line ~A ~S ~S~%" (number line)
          (debug-color line) (box-and-absolute (box line)))
  (format *trace-output* "  ~A band~:*~p~%" (length (bands line)))
  (with-bounds-and-clip-rect ((box line) clip-rect)
    (dolist (band (bands line))
      (draw band clip-rect))
    (dolist (measure (measures line))
      (draw measure clip-rect))
    (draw (number-annotation line) clip-rect)))


(defmethod draw ((page page) &optional clip-rect)
  (let* ((printable-area (paper-printable-area (partition page)))
         (margin (rect-inset printable-area -0.1 -0.1)))
    ;; Draw gray rect for printable area:
    (unless (and clip-rect
                 (rect-empty-p (rect-intersection clip-rect (left-side margin)))
                 (rect-empty-p (rect-intersection clip-rect (right-side margin)))
                 (rect-empty-p (rect-intersection clip-rect (bottom-side margin)))
                 (rect-empty-p (rect-intersection clip-rect (top-side margin))))
      (let ((p (create-path)))
        (add-rect p *transform* margin)
        (close-subpath p)
        (set-color :gray)
        (stroke-path p)))
    ;; Draw lines:
    (with-bounds-and-clip-rect ((box page) clip-rect)
      (dolist (line (lines page))
        (draw line clip-rect))
      (draw (number-annotation page) clip-rect)
      (let ((title (title-annotation page)))
        (when title
          (draw title clip-rect))))))


;;;; THE END ;;;;
