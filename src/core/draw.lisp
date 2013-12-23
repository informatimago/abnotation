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
;;;;    

(in-package "ABNOTATION.CORE")

(defmethod draw ((line line) &optional clip-rect)
  ;; draw portée
  ;; draw clef
  ;; draw measures
  ;; draw line number
  )

(defmethod draw ((page page) &optional clip-rect)
  (let ((margin (rect-inset (apply (function rect)
                                   (paper-printable-area (partition page))) -0.1 -0.1)))
    (unless (and clip-rect
                 (rect-empty-p (rect-intersection clip-rect (left-side margin)))
                 (rect-empty-p (rect-intersection clip-rect (right-side margin)))
                 (rect-empty-p (rect-intersection clip-rect (bottom-side margin)))
                 (rect-empty-p (rect-intersection clip-rect (top-side margin))))
      (let ((p (create-path)))
        (add-rect p nil margin)
        (close-subpath p)
        (set-color "gray")
        (stroke-path p))))
  ;; draw partition annotation and title on first page.
  (dolist (line (lines page))
    (draw line clip-rect))
  ;; draw page number
  ;; draw page annotation(s)
  )
