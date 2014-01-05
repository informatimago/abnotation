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
(in-package "ABNOTATION.CORE")

;; ((setf tempo-and-notes) (abnotation-sequence partition)):
;; [load midi] --> (list (or note tempo)) --> [splice-measure] --> (list tempo->measure->sound)
;;
;; (list tempo->measure->sound) --> [layout] --> (and boxes
;;                                                    page->line->measure->sound)


;; measure-box.width  = (or manual-adjustment (max (* tempo measure-speed) (max sound-box.width)))
;; measure-box.height = (or manual-adjustment (max sound-box.height))
;; 
;; line-box.width       = (- printable-area.width left-margin manual-adjustment)
;; (accumulate (measure) (< (sum measure-box.width) line-box.width))
;; top-ledger.top       = (reduce max  (measures line) :key measure.top)
;; bottom-ledger.bottom = (reduce min (measures line) :key measure.bottom)
;; line-box.height      = (- top-ledger.top bottom-ledger.bottom)accu
;; (accumulate (line) (< (sum (+ interline line.height)) page.height))



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

(defmethod compute-box-size ((band band) (partition partition))
  (let* ((line        (line band))
         (lane-height (/ (staff-height partition) 4))
         (lanes       (- (maximum-lane band) (minimum-lane band) -1))
         (height      (* 1/2 (1+ lanes) lane-height)))
    (setf (box-size band) (size (width (box line)) height))))

(defmethod compute-box-size ((line line) (partition partition))
  (let* ((lane-height (/ (staff-height partition) 4))
         (lanes       (- (maximum-lane (first (last (bands line))))
                         (minimum-lane (first (bands line)))
                         -1))
         (height      (* 1/2 (1+ lanes) lane-height)))
    (unless (= 1 (number line))
      (incf height (interline partition)))
    ;; (format *trace-output*
    ;;         "lane-height ~S  lanes ~S  height ~S  interline ~S~%"
    ;;         (coerce lane-height 'double-float)
    ;;         (coerce lanes 'double-float)
    ;;         (coerce height 'double-float)
    ;;         (interline partition))
    (setf (box-size line) (size (width (box (page line))) height))))



(defun remove-pages (partition)
  (dolist (page (pages partition))
    (dolist (line (lines page))
      (dolist (measure (measures line))
        (dolist (sound (sounds measure))
          (detach 'measure-contains measure sound))
        (detach 'line-contains-vertically line measure))
      (detach 'page-contains page line))
    (detach 'partition-contains partition page)))


(defmethod layout-partition-from-tempos ((partition partition))
  (let ((measure-speed (default-measure-speed partition))
        (measures '()))
    ;; compute measure widths:
    (dolist (tempo (tempos partition))
      (let* ((duration (measure-duration tempo))
             (width (* measure-speed duration)))
        (dolist (measure (measures tempo))
          (setf (box-size measure) (size width (* 58/8 (staff-height partition))))
          (push measure measures))))
    ;; spread measures over lines
    (setf measures (nreverse measures))
    ;; (setf (measures partition) measures)
    (remove-pages partition)
    (let ((pages '()) (pageno 0) page          
          (lines '()) (lineno 0) line
          (page-height 0)
          (lines-height 0))
      (labels ((new-line ()
                         (setf line (make-instance 'line :number (incf lineno)))
                         (push line lines)
                         (dolist (band (create-bands (staff-set partition)))
                           (attach 'line-contains-horizontally line band)))
               (new-page ()
                         (setf page (make-instance 'page :number (incf pageno)))
                         (push page pages)
                         (attach 'partition-contains partition page)
                         (attach 'page-contains page line)
                         (compute-box-size line partition)
                         (setf page-height  (height (box page))
                               lines-height (+ 20 #|title header|# (height (box line)))
                               (bottom (box line)) (- page-height lines-height))
                         (format *trace-output* "bottom line = ~S / ~S~%"
                                 (coerce (- page-height lines-height) 'double-float)
                                 (bottom (box line)))))
        (new-line)
        (loop
         :while measures
         :initially (new-page)
         :do (loop
              :with line-width = (width (box line))
              :with measures-width = 10 #|(width (clef line))|#
              :while measures
              :do (let ((measure (first measures)))
                    (if (< (+ (width (box measure)) measures-width)
                           line-width)
                      (progn
                        (setf (left (box measure)) measures-width)
                        (incf measures-width (width (box measure)))
                        (setf (line measure) nil)
                        (attach 'line-contains-vertically line measure)
                        (pop measures))
                      (progn
                        (new-line)
                        (if (< (+ (height (box line)) lines-height)
                               page-height)
                          (progn
                            (setf (page line) nil)
                            (attach 'page-contains page line)
                            (compute-box-size line partition)
                            (incf lines-height (height (box line)))
                            (setf (bottom (box line)) (- page-height lines-height))
                            (format *trace-output* "bottom line = ~S / ~S~%"
                                    (coerce (- page-height lines-height) 'double-float)
                                    (bottom (box line))))
                          (new-page)))))))))))


;;;; THE END ;;;;
