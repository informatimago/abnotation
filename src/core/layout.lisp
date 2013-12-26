;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               layout.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Lay out the partition.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-12-26 <PJB> Created.
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
;; ((setf tempo-and-notes) (abnotation-sequence partition)):
;; [load midi] --> (list (or note tempo)) --> [splice-measure] --> (list tempo->measure->sound)
;;
;; (list tempo->measure->sound) --> [layout] --> (and boxes
;;                                                    page->line->measure->sound)


;; (defgeneric layout (object children))
;; 
;; (defmethod layout ((partition partition) measures)
;;   (setf (pages partition) (loop
;;                             :for pnumber :from 1
;;                             :while measures
;;                             :collect (let ((page (make-instance 'page :number pnumber)))
;;                                        (attach 'partition-contains partition page)
;;                                        (setf measures (layout page measures))
;;                                        page)))
;;   measures)
;; 
;; 
;; (defmethod layout ((page page) measures)
;;   (let ((staff-set (staff-set (partition page)))
;;         (box (make-instance 'page-box :box (paper-printable-area (parameters (partition page)))))
;;         (boxes '())
;;         (lines '())
;;         (y (top (box box))))
;;     (loop
;;       :for lnumber :from 1
;;       :while measures
;;       :do (let ((line (make-instance 'line :number lnumber)))
;;             (attach 'page-contains page line)
;;             (dolist (band (create-bands staff-set))
;;               (attach 'line-contains-horizontally line band))
;;             (multiple-value-bind (line-box remaining-measures) (layout line measures)
;;               (setf measures remaining-measures)
;;               (decf y (height (box line-box)))
;;               ;; while (<= y (bottom (box box))
;;               (setf (rect-y (box line-box)) y)
;;               (push line-box boxes)
;;               (push line lines))))
;;     (setf (lines page) lines)
;;     (values 
;;             measures)))
;; 
;; (defmethod layout ((page page))
;;   
;;   (make-instance 'page-box
;;       :box box))



;; (/ 7.0 4)1.75
;; (/ 5.0 4)1.25
;; (/ 3.0 4)0.75
;; 
;; ---
;; ---
;; ---
;; ---
;; ---

(defmethod lay-partition-out ((partition partition))

  )


;;;; THE END ;;;;
