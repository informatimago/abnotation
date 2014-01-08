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


(defgeneric compute-box-size (element)
  (:documentation "Compute the size of the element.
cluster <- head, accidentals, tenue, duration
measure <- tempo staff (line)
line <- staff page
page <- paper size
"))

(defgeneric layout (element)
  (:documentation "Compute the position of the element relative to its container:
sounds (notes or clusters) also create the associated elements (head, accidental, & segments).
sound -> measure
measure -> line
line -> page
page ->
"))

(defmethod layout :before (element)
  (compute-box-size element))

(defgeneric move-over (element)
  (:documentation "Adjust the associations of the element according to required layout for the new attribute values.
cluster -> move over to another measure (perhaps add new measures).
measure -> move over to another line (perhaps add new lines).
line -> move over to another page (perhaps add new pages).
page ->
"))

(defgeneric did-change (element)
  (:documentation "Indicates the UI that the model element changed and needs to be redisplayed (if visible)."))

(defgeneric draw  (element &optional clip-rect)
  (:documentation "Draw the element (at least the part within the clip-rect).
Drawing is done by creating and stroking or filling bezier paths."))


#|

A- drawing notes in a measure.
------------------------------------------------------------

Each measure can have its own tempo: when editing notes in a measure,
we can put this edited measure in its own tempo and adjust it while
notes are added/removed/changed, and when edition is complete, we can
re-adjust the tempo and recompute the distribution of notes over the
measures.

Changes to the model should send a message to the controler/view so
that display is updated when the measure is visible.


A1: compute the measure duration (tempo)
- some notes can spread over to the following measures (usual case),
- some notes can be marked to stay within the measure (while editing).


A2: layout of notes is absolute, given the start and duration of the
measure, and the data of the note.



B- spreading notes over to measures.
------------------------------------------------------------

Starting from virgin state: DONE.

But we need to adjust also from an existing set of measures.  

Changes:

- move notes (forward backward time interval) ==> reassign notes to
  measures.

- change the tempo of a measure => change the start time of the
  following measures ==> reassign notes to measures.



C- spreading measures over to lines and lines to pages.
------------------------------------------------------------


|#


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



(defmethod compute-box-size ((head head))
  (let* ((partition (partition (page (line (measure head)))))
         (height    (lane-height partition))
         (excentricity 1.2))
    (setf (box-size head) (size (* excentricity height) height))))

(defmethod layout ((head head))
  (let* ((measure     (measure head))
         (partition   (partition (page (line measure))))
         (lane-height (lane-height partition))
         (lane        (lane (pitch (sound head)))))
    (setf (box-origin head) (point (- (start-time (sound head))
                                      (start-time measure)
                                      (width (box head)))
                                   (* 0.5 lane lane-height)))))



(defmethod compute-box-size ((accidental accidental))
  (let* ((partition (partition (page (line (measure head)))))
         (height    (lane-height partition))
         (excentricity 1.2))
    (setf (box-size head) (size (* excentricity height) (* excentricity height)))))

(defmethod layout ((accidental accidental))
  (let* ((measure     (measure head))
         (partition   (partition (page (line measure))))
         (lane-height (lane-height partition))
         (lane        (lane (pitch (sound head)))))
    (setf (box-origin head) (point (- (start-time (sound head))
                                      (start-time measure))
                                   (* 0.5 lane lane-height)))))



(defun segment-width (segment)
  (let ((measure (measure segment)))
    (cond
      ((first-segment-p segment measure)
       (- (end-time measure) (start-time segment)))
      ((last-segment-p segment measure)
       (- (end-time segment) (start-time measure)))
      (t
       (measure-duration measure)))))

(defun tenue-height (segment)
  "height of a tenue line (in millimeter)."
  ;; TODO: should depend on the height of the staves
  0.1)

(defun dynamic-height (segment)
  "height of a dynamic (in millimeter)."
  ;; TODO: should depend on the height of the staves
  3.0)

(defun beam-height (segment)
  "height of a beam line (in millimeter)."
  ;; TODO: should depend on the height of the staves
  0.5)

;; beam dynamic<> and tenue, and annotation, could span several measures/lines/pages.
(defmethod compute-box-size ((segment tenue-segment))
  (setf (box-size segment) (size (segment-width segment)
                                 (tenue-height segment))))

(defmethod compute-box-size ((segment dynamic-segment))
  (setf (box-size segment) (size (segment-width segment)
                                 (dynamic-height segment))))

(defmethod compute-box-size ((segment beam-segment))
  (setf (box-size segment) (size (segment-width segment)
                                 (beam-height segment))))




(defmethod compute-box-size ((measure measure))
  (let* ((partition     (partition (page (line measure))))
         (measure-speed (default-measure-speed partition))
         (tempo         (tempo measure))
         (duration      (measure-duration tempo))
         (width         (* measure-speed duration)))
    (setf (box-size measure) (size width (* 58/8 (staff-height partition))))))

  
(defmethod compute-box-size ((band band))
  (let* ((line        (line band))
         (partition   (partition (page line)))
         (lane-height (/ (staff-height partition) 4))
         (lanes       (- (maximum-lane band) (minimum-lane band) -1))
         (height      (* 1/2 (1+ lanes) lane-height)))
    (setf (box-size band) (size (width (box line)) height))))

 
(defmethod compute-box-size ((line line))
  (let* ((partition   (partition (page line)))
         (lane-height (/ (staff-height partition) 4))
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


(defmethod compute-box-size ((page page))
  (setf (box page) (paper-printable-area (partition page))))  
  




  
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
                         (format *trace-output* "bottom line = ~S =/= ~S~%"
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
                    (when (<= line-width (+ (width (box measure)) measures-width))
                      (new-line)
                      (if (< (+ (height (box line)) lines-height)
                             page-height)
                          (progn
                            (setf (page line) nil)
                            (attach 'page-contains page line)
                            (compute-box-size line partition)
                            (incf lines-height (height (box line)))
                            (setf (bottom (box line)) (- page-height lines-height))
                            (format *trace-output* "bottom line = ~S =?= ~S~%"
                                    (coerce (- page-height lines-height) 'double-float)
                                    (bottom (box line))))
                          (new-page)))
                    ;; attach measure to line:
                    (setf (left (box measure)) measures-width)
                    (incf measures-width (width (box measure)))
                    (setf (line measure) nil)
                    (attach 'line-contains-vertically line measure)
                    (pop measures))))))))


;;;; THE END ;;;;
