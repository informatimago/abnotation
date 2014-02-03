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
cluster <- tete, accidentals, tenue, duration
measure <- tempo staff (line)
line <- staff page
page <- paper size
"))

(defgeneric layout (element)
  (:documentation "Compute the position of the element relative to its container:
sounds (notes or clusters) also create the associated elements (tete, accidental, & segments).
sound -> measure
measure -> line
line -> page
page ->
"))

(defmethod layout :before ((element graphic-element))
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


A1: measure duration
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



(defmethod compute-box-size ((tete tete))
  (let* ((partition (partition (page (line (measure (sound tete))))))
         (height    (lane-height partition))
         (excentricity 1.2))
    (setf (box-size tete) (size (* excentricity height) height))))

(defmethod layout ((tete tete))
  (let* ((measure     (measure (sound tete)))
         (partition   (partition (page (line measure))))
         (lane-height (lane-height partition))
         (lane        (lane (pitch (sound tete)))))
    (setf (box-origin tete)
          (vector+ (point (- (start-time (sound tete))
                             (start-time measure)
                             (width (box tete)))
                          (* 0.5 lane lane-height))
                   (offset tete)))))


(defun lane-bottom (pitch partition)
  "The bottom of a lane at pitch in a line of the partition."
  (let ((lane-height (lane-height partition))
        (lane        (lane pitch)))
    (* 0.5 lane lane-height)))

(defmethod compute-box-size ((accidental accidental))
  (let* ((partition (partition (page (line (measure (sound accidental))))))
         (height    (lane-height partition))
         (excentricity 1.2))
    (setf (box-size accidental) (size (* excentricity height) (* excentricity height)))))

(defmethod layout ((accidental accidental))
  (let* ((sound       (sound accidental))
         (measure     (measure sound))
         (partition   (partition (page (line measure)))))
    (setf (box-origin accidental)
          (vector+ (point (- (start-time sound)
                             (start-time measure))
                          (lane-bottom (pitch sound) partition))
                   (offset accidental)))))


(defun segment-width (segment)
  (let ((measure (measure segment)))
    (cond
      ((first-segment-p segment)
       (- (end-time measure) (start-time segment)))
      ((last-segment-p segment)
       (- (end-time segment) (start-time measure)))
      (t
       (duration measure)))))

(defun tenue-height (segment)
  "height of a tenue line (in millimeter)."
  ;; TODO: should depend on the height of the staves
  (declare (ignore segment))
  0.1)

(defun dynamic-height (segment)
  "height of a dynamic (in millimeter)."
  ;; TODO: should depend on the height of the staves
  (declare (ignore segment))
  3.0)

(defun beam-height (segment)
  "height of a beam line (in millimeter)."
  ;; TODO: should depend on the height of the staves
  (declare (ignore segment))
  0.5)

;; beam dynamic<> and tenue, and annotation, could span several measures/lines/pages.

(defun tenue-offset (partition)
  "Position of the tenue above the maximum lane."
  ;; TODO: should depend on the height of the staves
  (declare (ignore partition))
  0.2)

(defmethod compute-box-size ((segment beam-segment))
  (setf (box-size segment) (size (segment-width segment)
                                 (beam-height segment))))

(defmethod layout ((segment tenue-segment))
  (let* ((line (line (measure segment)))
         (partition (partition (page line))))
    (setf (box-origin segment)
          (vector+ (point (segment-width segment)
                          (+ (lane-bottom (maximum-lane line) partition)
                             (lane-height partition)
                             ;; TODO: tenue offsets depend on the sound, they can be configured manually.
                             (tenue-offset partition)))
                   (offset segment)))))


(defmethod compute-box-size ((segment tenue-segment))
  (setf (box-size segment) (size (segment-width segment)
                                 (tenue-height segment))))

(defmethod layout ((segment tenue-segment))
  (let* ((line (line (measure segment)))
         (partition (partition (page line))))
    (setf (box-origin segment)
          (vector+ (point (segment-width segment)
                          (+ (lane-bottom (pitch (sound segment)) partition)
                             (* 0.35 (lane-height partition))))
                   (offset segment)))))


(defmethod compute-box-size ((segment dynamic-segment))
  (setf (box-size segment) (size (segment-width segment)
                                 (dynamic-height segment))))

(defmethod layout ((segment dynamic-segment))
  (let ((partition     (partition (page (line (measure segment))))))
    (setf (box-origin segment)
          (vector+ (point (segment-width segment)
                          (- (lane-bottom 0 partition)
                             (height (box segment))))
                   (offset segment)))))

;;--------
;;
;; Note: since sound (note and cluster) is not a graphic-element, but
;;       a mere element, there's no layout :before method to
;;       compute-box-size.
;;

(defmethod compute-box-size ((sound sound))
  )

(defmethod layout :after ((sound sound))
  (compute-box-size sound))

(defmethod layout ((sound sound))
  ;; start-time --> tete & beam position
  (beam-segments sound)
  ;; duration --> beam segments
  (beam-segments sound)
  ;; dynamic --> dynamic segments
  (dynamic-segments sound)
  ;; /end-time vs. (start-time (next sound)) --> tenue
  )

(defmethod layout ((note note))
  (call-next-method)
  ;; pitch --> tete
  (unless (tete note)
    (setf  (tete note) (make-instance 'tete)))
  (layout tete)
  ;; pitch --> accidental (0-1)
  (let ((accidental (accidental (pitch note))))
    (unless (eql :natural accidental)
      (unless (and (accidental note) (not))
        (setf (accidental note) (make-instance 'accidental :character accidental)))
      (layout accidental))))

(defmethod layout ((cluster cluster))
  (call-next-method)
  ;; notes
  (dolist (note (notes cluster))
    (layout note)))

;;
;;--------

(defmethod compute-box-size ((measure measure))
  (let* ((partition     (partition (page (line measure))))
         (measure-speed (default-measure-speed partition))
         (duration      (duration measure))
         (width         (* measure-speed duration)))
    (setf (box-size measure) (size width (* 58/8 (staff-height partition))))))

(defmethod layout ((measure measure))
  (let ((line (line measure)))
    (setf (box-origin measure)
          (if (first-element-in-container-p measure)
              (point (measure-left-position line) 0)
              (point (right (box (previous measure))) 0)))
    (dolist (sound (sounds measure))
      (layout sound))))


(defmethod compute-box-size ((band band))
  (let* ((line        (line band))
         (partition   (partition (page line)))
         (lane-height (/ (staff-height partition) 4))
         (lanes       (- (maximum-lane band) (minimum-lane band) -1))
         (height      (* 1/2 (1+ lanes) lane-height)))
    (setf (box-size band) (size (width (box line)) height))))

(defmethod layout ((band band))
  (let* ((line        (line band))
         (partition   (partition (page line)))
         (lane-height (/ (staff-height partition) 4))
         (base        (* 1/2 (minimum-lane band) lane-height)))
    (setf (box-origin band) (point 0 base))))

(defmethod layout ((staff staff))
  (when (next-method-p) (call-next-method))
  (layout (clef staff)))


(defmethod compute-box-size ((clef clef))
  (setf (box-size clef) (size 10.0 (height (box (staff clef))))))

(defmethod layout ((clef clef))
  (setf (box-origin clef) (point 0.0 0.0))0)


(defmethod measure-left-position ((line line))
  (reduce (function max) (bands line)
          :key (lambda (band)
                 (typecase band
                   (staff (right (box (clef band))))
                   (t 0)))))

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

(defmethod layout ((line line))
  (setf (box-origin line)
        (vector+ (point 0 (- (if (first-element-in-container-p line)
                                 (top (box (page line)))
                                 (bottom (box (previous line))))
                             (height (box line))))
                 (offset line)))
  (dolist (band (bands line))
    (layout band))
  (dolist (measure (measures line))
    (layout measure)))



(defmethod compute-box-size ((page page))
  (setf (box page) (paper-printable-area (partition page))))

(defmethod layout ((page page))
  (setf (box page) (paper-printable-area (partition page)))
  (dolist (line (lines page))
    (layout line)))



(defmethod compute-box-size ((partition partition))
  (values))

(defmethod layout ((partition partition))
  (dolist (page (pages partition))
    (layout page)))


(defun remove-pages (partition)
  (declare (ignore partition))
  #+TODO
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
        ;; (measures '())
        )
    ;; compute measure widths:
    ;; (dolist (tempo (tempos partition))
    ;;   (let* ((duration (duration tempo))
    ;;          (width (* measure-speed duration)))
    ;;     (dolist (measure (measures tempo))
    ;;       (setf (box-size measure) (size width (* 58/8 (staff-height partition))))
    ;;       (push measure measures))))
    ;; spread measures over lines
    ;; (setf measures (nreverse measures))
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
                 (span-append-node partition page) ; (attach 'partition-contains partition page)
                 (span-append-node page line) ;  (attach 'page-contains page line)
                 (compute-box-size line)
                 (compute-box-size page)
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
                              (span-append-node page (extract-node line)) ; (setf (page line) nil) (attach 'page-contains page line)
                              (compute-box-size line)
                              (incf lines-height (height (box line)))
                              (setf (bottom (box line)) (- page-height lines-height))
                              (format *trace-output* "bottom line = ~S =?= ~S~%"
                                      (coerce (- page-height lines-height) 'double-float)
                                      (bottom (box line))))
                            (new-page)))
                      ;; attach measure to line:
                      (setf (left (box measure)) measures-width)
                      (incf measures-width (width (box measure)))
                      (span-append-node line (extract-node measure)) ; (setf (line measure) nil) (attach 'line-contains-vertically line measure)
                      (pop measures))))))))


;;;; THE END ;;;;
