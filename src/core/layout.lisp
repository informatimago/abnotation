
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

#||

||#
