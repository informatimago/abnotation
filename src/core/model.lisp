;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               model.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;     The model of the ABNotation.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-12-11 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2013 - 2013
;;;;    
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************

(in-package "ABNOTATION.CORE")

(defvar *partition* nil
  "The current partition.")

;; (NOTE pitch start-time duration intensity)
;; 
;; 
;; 
;; 
;; 
;; 
;;    brace ---- 1..4 staff




;;                  element 1------------------o annotation
;;                     ^                             ^                             
;;                     |                             |
;;                     |                     +-------+--------+
;;  +------------------|----* tempo 1----+   |                |
;;  |                  |                 | image             text
;;  |  +------------+--+-----+---------+ |
;;  |  |            |        |         | *
;; partition 1--* page 1--* line 1--* measure 1---* sound
;;                           | \                      ^
;;                           *  *                     |
;;                       staff  ledger        +-------+-------+
;;                       |   o    o           |               |
;;                       |   |    |           |               |
;;                     clef  +----========* note *-------o cluster


(defclass element ()
  ())

(defclass graphic-element ()
  ((box :initarg :box :accessor box :type rect)))

(defclass offsetable-element (graphic-element)
  ((offset :initarg :offset :accessor offset :type point)))


(defclass annotation (offsetable-element)
  ())

(defclass image (annotation)
  ((filename :initarg :filename :accessor filename
             :type string
             :documentation "The filename of the file in the partition
                             package file where the image is saved in.")))

(defclass text (annotation)
  ((text :initarg :text :accessor text :type string :initform ""
         :documentation "Some rich text string representation.")))

(define-association annotation
    ((element :type element
              :multiplicity #|1|# 0-1
              :kind :aggregation)
     (annotation :type annotation
                 :multiplicity 0-1)))


(defclass sound (element)
  ((start-time :initarg :start-time :accessor start-time )
   (duration  :initarg :duration :accessor duration)
   (dynamic :initarg :dynamic :accessor dynamic :initform :mf)))

(defclass note (sound)
  ((pitch :initarg :pitch :accessor pitch)))


(defclass cluster (sound)
  ())

(define-association groups
    ((cluster :type cluster
              :multiplicity #|1|# 0-1
              :kind :aggregation)
     (notes :type note
            :multiplicity 0-*)))


(defclass head (offsetable-element)
  ())

(defclass accidental (offsetable-element)
  ()
  (:documentation "Dieze, bemol ou becare devant la head."))

(define-association note-head
    ((head :type head
           :multiplicity #|1|# 0-1)
     (note :type note
           :multiplicity #|1|# 0-1)))

(define-association note-accidental
    ((accidental :type accidental
                 :multiplicity 0-1)
     (note :type note
           :multiplicity #|1|# 0-1)))

;; beam dynamic<> and tenue, and annotation, could span several measures/lines/pages.
(defclass beam-segment (offsetable-element)
  ())
(defclass dynamic-segment (offsetable-element)
  ())
(defclass tenue-segment (offsetable-element)
  ())

(define-association sound-beams
    ((beam-segments :type beam-segment
                    :multiplicity 0-*
                    :ordered t)
     (sound :type sound
            :multiplicity #|1|# 0-1)))
(define-association sound-dynamics
    ((dynamic-segments :type dynamic-segment
                       :multiplicity 0-*
                       :ordered t)
     (sound :type sound
            :multiplicity #|1|# 0-1)))
(define-association sound-tenues
    ((tenue-segments :type tenue-segment
                     :multiplicity 0-*
                     :ordered t)
     (sound :type sound
            :multiplicity #|1-*|# 0-*))
  (:documentation "When a tenue has several sounds, then it's a ------ tenue.
Otherwise it's a - - - - tenue."))


(defclass numbered ()
  ((number :initarg :number :reader number)))

(defgeneric renumber (numbered))


(defclass measure (graphic-element numbered)
  ((stat-time      :initarg :start-time     :accessor start-time     :initform 0)
   (adjusted-width :initarg :adjusted-width :accessor adjusted-width :type coordinate)
   (front-kerning  :initarg :front-kerning  :accessor front-kerning  :type coordinate)))

(defmethod end-time ((measure measure))
  (+ (start-time measure) (measure-duration (tempo measure))))

(define-association measure-contains
    ((measures :type measure
               :multiplicity #|1-*|# 0-*)
     (sounds :type sound
             :multiplicity 0-*
             :ordered t))
  (:documentation "A sound can span over several measures.  The head
is on the first one, but the heam, dynamic and tenue can have several
segments, one on each successive measure."))


(defclass line (offsetable-element numbered)
  ())

(define-association line-contains-vertically
    ((line :type line
           :multiplicity #|1|# 0-1
           :kind :aggregation)
     (measures :type measure
               :multiplicity 0-*
               :ordered t)))


(defclass band (graphic-element)
  ())

(define-association line-contains-horizontally
    ((line :type line
          :multiplicity #|1|# 0-1
           :kind :aggregation)
     (bands :type band
            :multiplicity #|1-*|# 0-*
            :ordered t)))

(define-association band-contains
    ((band :type band
           :multiplicity #|1|# 0-1)
     (notes :type note
            :multiplicity #|1-*|# 0-*)))

;; Those four generic functions have methods for staff, ledger and clef.  The staff methods defer to the (clef staff).
(defgeneric maximum-pitch (element) 
  (:documentation "The maximum pitch of a note on the ledger above a staff with the clef."))
(defgeneric top-pitch (element)
  (:documentation "The pitch of the top line of a staff with the clef."))
(defgeneric bottom-pitch (element)
  (:documentation "The pitch of the bottom line of a staff with the clef."))
(defgeneric minimum-pitch (element)
  (:documentation "The minimum pitch of a note on the ledger below a staff with the clef."))


(defclass ledger (band)
  ((minimum-pitch :initarg :minimum-pitch :reader minimum-pitch)
   (bottom-pitch  :initarg :bottom-pitch  :reader bottom-pitch)
   (top-pitch     :initarg :top-pitch     :reader top-pitch)
   (maximum-pitch :initarg :maximum-pitch :reader maximum-pitch))
  (:documentation "The lines between the staves."))

(defclass staff (band)
  ())

(defclass clef (graphic-element)
  ((name :initarg :name :reader name)
   (line :initarg :line :reader line
         :documentation "The line of the staff on which the clef is aligned (1-5, 1=bottom).")
   (pitch :initarg :pitch :reader pitch
          :documentation "The pitch of the clef = the note on (line clef).")))

(define-association gives-pitch
    ((clef :type clef
           :multiplicity #|1|# 0-1)
     (staff :type staff
            :multiplicity #|1|# 0-1
            :kind :aggregation)))




(defclass page (graphic-element numbered)
  ())

(define-association page-contains
    ((page :type page
           :multiplicity #|1|# 0-1
           :kind :aggregation)
     (lines :type line
            :multiplicity 0-*
            :ordered t)))


(defclass partition ()
  ((title                 :initarg :title                 :accessor title
                          :type string                    :initform "untitled")
   (author                :initarg :author                :accessor author
                          :type string                    :initform "anonymous")
   (file                  :initarg :file                  :accessor file
                          :type (or pathname null)        :initform nil)
   (staff-set             :initarg :staff-set             :accessor staff-set)
   (page-number-font      :initarg :page-number-font      :accessor page-number-font
                          :type string)
   (line-number-font      :initarg :line-number-font      :accessor line-number-font
                          :type string)
   (measure-number-font   :initarg :measure-number-font   :accessor measure-number-font
                          :type string)
   (default-measure-speed :initarg :default-measure-speed :accessor default-measure-speed
                          :type real :initform 40
                          :documentation "The scale of a measure in mm/s.")
   (paper-format          :initarg :paper-format          :accessor paper-format
                          :type string)
   (paper-orientation     :initarg :paper-orientation     :accessor paper-orientation
                          :type (member :portrait :paysage) :initform :portrait)
   (paper-size            :initarg :paper-size            :accessor paper-size
                          :type list
                          :documentation "The (width height) in millimeter of the paper page.")
   (paper-printable-area  :initarg :paper-printable-area  :accessor paper-printable-area
                          :type list
                          :documentation "The (left bottom width height) in millimeter of the printable area.")
   (staff-height          :initarg :staff-height          :accessor staff-height
                          :type real
                          :documentation "Unit: millimeter, values: 3, 5, 7 mm")))

(defparameter *papers*
  '((("A4" :portrait) (210 297) (10 10 190 277))
    (("A4" :paysage)  (297 210) (10 10 277 190))
    (("A3" :portrait) (297 420) (10 10 277 4000))
    (("A3" :paysage)  (420 297) (10 10 400 277))))

(defun paper-size-and-printable-area (format orientation)
  (values-list (cdr (assoc (list format orientation) *papers* :test (function equalp)))))



(defmethod initialize-instance :after ((partition partition) &rest args &key &allow-other-keys)
  (when args
   (unless (and (slot-boundp partition 'paper-size)
                (slot-boundp partition 'paper-printable-area))
     (unless (and (slot-boundp partition 'paper-format)
                  (slot-boundp partition 'paper-orientation))
       (error "Either the :paper-format and :paper-orientation must be given, ~
              or :paper-size and :paper-printable-area must be given."))
     (setf (values (slot-value partition 'paper-size)
                   (slot-value partition 'paper-printable-area))
           (paper-size-and-printable-area  (slot-value partition 'paper-format)
                                           (slot-value partition 'paper-orientation)))))
  partition)


(define-association partition-contains
    ((partition :type partition
                :multiplicity #|1|# 0-1
                :kind :aggregation)
     (pages :type page
            :multiplicity 0-*
            :ordered t)))


(defclass tempo (element)
  ((measure-duration :initarg :measure-duration :accessor measure-duration)))

(define-association partition-tempo
    ((partition :type partition
                :multiplicity #|1|# 0-1
                :kind :aggregation)
     (tempos :type tempo
             :multiplicity 0-*
             :ordered t)))

(define-association gives-tempo
    ((tempo :type tempo
            :multiplicity #|1|# 0-1)
     (measures :type measure
               :multiplicity 0-*
               :ordered t)))





(defmacro define-print-object (class &rest slots)
  `(defmethod print-object ((object ,class) stream)
    (print-parseable-object (object stream :type t :identity t) ,@slots)))

(define-print-object image   filename)
(define-print-object text    text)
(define-print-object note    start-time duration dynamic pitch)
(define-print-object cluster start-time duration dynamic notes)
(define-print-object measure number sounds)
(define-print-object line    number bands measures)
(define-print-object page    number lines)
(define-print-object ledger  minimum-pitch bottom-pitch top-pitch maximum-pitch)
(define-print-object staff   clef)
(define-print-object clef    name line pitch)
(define-print-object tempo   measure-duration measures)
(define-print-object partition
    title author file staff-set pages tempos
    page-number-font line-number-font measure-number-font
    paper-format paper-orientation
    paper-size paper-printable-area
    staff-height)


(defun create-bands (staff-set)
  (let ((clefs      (list (make-instance 'clef :name :bass15mb   :line 4 :pitch  41)
                          (make-instance 'clef :name :bass       :line 4 :pitch  65)
                          (make-instance 'clef :name :treble     :line 2 :pitch  79)
                          (make-instance 'clef :name :treble15ma :line 2 :pitch 103))))
    (subseq (list (make-instance 'ledger
                      :minimum-pitch 23 :bottom-pitch 24
                      :maximum-pitch 28 :top-pitch 28)
                  (let ((staff (make-instance 'staff)))
                    (attach 'gives-pitch (pop clefs) staff)
                    staff)
                  (make-instance 'ledger
                      :minimum-pitch 48 :bottom-pitch 48
                      :maximum-pitch 52 :top-pitch 52)
                  (let ((staff (make-instance 'staff)))
                    (attach 'gives-pitch (pop clefs) staff)
                    staff)
                  (make-instance 'ledger
                      :minimum-pitch 72 :bottom-pitch 72
                      :maximum-pitch 72 :top-pitch 72)
                  (let ((staff (make-instance 'staff)))
                    (attach 'gives-pitch (pop clefs) staff)
                    staff)
                  (make-instance 'ledger
                      :minimum-pitch 93 :bottom-pitch 93
                      :maximum-pitch 96 :top-pitch 96)
                  (let ((staff (make-instance 'staff)))
                    (attach 'gives-pitch (pop clefs) staff)
                    staff)
                  (make-instance 'ledger
                      :minimum-pitch 117 :bottom-pitch 117
                      :maximum-pitch 122 :top-pitch 121))
            (first staff-set) (second staff-set))))


(defun create-partition (staff-set &key (title "untitled") (author "anonymous") parameters)
  (let* ((partition  (make-instance 'partition
                         :title title
                         :author author
                         :staff-set staff-set
                         :title-number-font "Helvetica-16"
                         :page-number-font "Helvetica-12"
                         :line-number-font "Helvetica-10"
                         :measure-number-font "Helvetica-8"
                         :paper-format "A4"
                         :paper-orientation :portrait
                         :staff-height 5))
         (*partition* partition)
         (tempo      (make-instance 'tempo :measure-duration 1))
         (page       (make-instance 'page :number 1))
         (line       (make-instance 'line :number 1))
         (measure    (make-instance 'measure :number 1)))
    ;; (attach 'measure-contains measure sound)
    ;; (attach 'band-contains band note)
    (attach 'line-contains-vertically line measure)
    (dolist (band (create-bands staff-set))
      (attach 'line-contains-horizontally line band))
    (attach 'page-contains page line)
    (attach 'partition-contains partition page)
    (attach 'partition-tempo partition tempo)
    (attach 'gives-tempo tempo measure)
    partition))


(defparameter *staves/bass*                 '(2 5))
(defparameter *staves/trebble*              '(4 7))
(defparameter *staves/bass-trebble*         '(2 7))
(defparameter *staves/bass-trebble15ma*     '(2 nil))
(defparameter *staves/bass15mb-trebble*     '(0 7))
(defparameter *staves/bass15mb-trebble15ma* '(0 nil))

;;;; THE END ;;;;
