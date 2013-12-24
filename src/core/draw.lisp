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



(defmethod draw ((line line) &optional clip-rect)
  (declare (ignore clip-rect))
  ;; draw portée
  ;; draw clef
  ;; draw measures
  ;; draw line number
  )

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
    ;; draw partition annotation and title on first page.
    (dolist (line (lines page))
      (draw line clip-rect))
    ;; draw page number
    ;; draw page annotation(s)
    (draw-string "Hello World" (point (left printable-area) (- (top printable-area) 20.0)))

    (set-font "Maestro" 12.0)
    (draw-string (map 'string 'code-char '(104 113 101 120))
                 (point (left printable-area) (- (top printable-area) 60.0))
                 :attributes t)
    (let ((p (create-path)))
      (move-to-coordinates p nil (left printable-area) (- (top printable-area) 60.0))
      (line-to-coordinates p nil (right printable-area) (- (top printable-area) 60.0))
      (stroke-path p))
    ))
