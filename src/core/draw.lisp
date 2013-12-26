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

(defmethod draw ((r rect) &optional clip-rect)
  (declare (ignore clip-rect))
  (let* ((p      (create-path))
         (left   (rect-left   r))
         (right  (rect-right  r))
         (top    (rect-top    r))
         (bottom (rect-bottom r))
         (thick  0.1 #|mm|#)) 
    (move-to-coordinates p nil left bottom)
    (line-to-coordinates p nil left top)
    (line-to-coordinates p nil right top)
    (line-to-coordinates p nil right bottom)
    (line-to-coordinates p nil left bottom)
    (move-to-coordinates p nil (+ left thick) (+ bottom thick))
    (line-to-coordinates p nil (- right thick) (+ bottom thick))
    (line-to-coordinates p nil (- right thick) (- top thick))
    (line-to-coordinates p nil (+ left thick) (- top thick))
    (line-to-coordinates p nil (+ left thick) (+ bottom thick))
    (close-subpath p)
    (fill-path p)))



(defmethod draw :before ((element graphic-element) &optional clip-rect)
  (set-color :red)
  (draw (box element) clip-rect))

(defmethod draw :after ((element graphic-element) &optional clip-rect)
  (when (annotation element)
   (draw (annotation element) clip-rect)))


(defmethod draw ((element image) &optional clip-rect)
  ;; TODO: draw image in (box element)
  )

(defmethod draw ((element text) &optional clip-rect)
  ;; TODO: draw rich text in (box element)
  )


(defmethod draw ((element head) &optional clip-rect)
  (set-color :black)
  #| TODO: draw ellipse in box |#)

(defmethod draw ((element accidental) &optional clip-rect)
  (set-color :black)
  #| TODO: draw ♮♯♭ in box |#)

(defmethod draw ((element tenue-segment) &optional clip-rect)
  (set-color :black)
  #| TODO: draw tenue segment in box |#)

(defmethod draw ((element dynamic-segment) &optional clip-rect)
  (set-color :black)
  #| TODO: draw dynamic segment in box |#)

(defmethod draw ((element beam-segment) &optional clip-rect)
  (set-color :black)
  #| TODO: draw beam segment in box |#)


(defmethod on-line-p ((note note))
  ;; TODO: (f (pitch note))
  nil)

(defmethod draw ((element ledger) &optional clip-rect)
  (set-color :black)
  (dolist (note (notes element))
    (when (on-line-p note)
      ;; (draw-ledger-line (rect-inset (box (head note)) -1.0 0))
      )))

(defmethod draw ((element staff) &optional clip-rect)
  (set-color :black)
  ;; TODO:
  ;; (loop
  ;;  :with box = (box element)
  ;;  :for y :from 0 :to 4
  ;;  :do (draw-line (left box) y (right box) y))
  (draw (clef element) clip-rect))

(defmethod draw ((element clef) &optional clip-rect)
  #|TODO: draw clef|#)

(defmethod draw ((element measure) &optional clip-rect)
  ;; TODO: (draw-measure-hat element clip-rect)
  ;; TODO: (draw-measure-bar element clip-rect)
  (draw (number-annotation element) clip-rect))

(defmethod draw ((element line) &optional clip-rect)
  (dolist (band (bands element))
    (draw band clip-rect))
  (dolist (measure (measures element))
    (draw measure clip-rect))
  (draw (number-annotation element) clip-rect))

(defmethod draw ((page page) &optional clip-rect)
  (let* ((printable-area (apply (function rect) (paper-printable-area (partition page))))
         (margin (rect-inset printable-area -0.1 -0.1)))
    (unless (and clip-rect
                 (rect-empty-p (rect-intersection clip-rect (left-side margin)))
                 (rect-empty-p (rect-intersection clip-rect (right-side margin)))
                 (rect-empty-p (rect-intersection clip-rect (bottom-side margin)))
                 (rect-empty-p (rect-intersection clip-rect (top-side margin))))
      (let ((p (create-path)))
        (add-rect p nil margin)
        (close-subpath p)
        (set-color :gray)
        (stroke-path p)))
    (dolist (line (lines page))
      (draw line clip-rect))
    (draw (number-annotation element) clip-rect)
    (let ((title (title-annotation  element)))
      (when title
        (draw title clip-rect)))
    (progn ; debug
      (draw-string "Hello World" (point (left printable-area) (- (top printable-area) 20.0)))
      (set-font "Maestro" 12.0)
      (draw-string (map 'string 'code-char '(104 113 101 120))
                   (point (left printable-area) (- (top printable-area) 60.0))
                   :attributes t)
      (let ((p (create-path)))
        (move-to-coordinates p nil (left printable-area) (- (top printable-area) 60.0))
        (line-to-coordinates p nil (right printable-area) (- (top printable-area) 60.0))
        (stroke-path p)))))

;;;; THE END ;;;;
