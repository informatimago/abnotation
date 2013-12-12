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

(defclass annotation (element)
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
              :multiplicity 1
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
              :multiplicity 1
              :kind :aggregation)
     (note :type note
           :multiplicity 0-*)))


(defclass numbered ()
  ((number :initarg :number :reader number)))

(defgeneric renumber (numbered))


(defclass measure (element numbered)
  ())

(define-association measure-contains
    ((measure :type measure
              :multiplicity 1
              :kind :aggregation)
     (sound :type sound
            :multiplicity 0-*
            :ordered t)))

(defclass line (element numbered)
  ())

(define-association line-contains
    ((line :type line
           :multiplicity 1
           :kind :aggregation)
     (measure :type measure
              :multiplicity 0-*
              :ordered t)))

(defclass page (element numbered)
  ())

(define-association line-contains
    ((page :type page
           :multiplicity 1
           :kind :aggregation)
     (line :type line
           :multiplicity 0-*
           :ordered t)))

(defclass partition ()
  ((title :initarg :title :accessor title
          :type string :initform "untitled")
   (author :initarg :author :accessor author
           :type string :initform "anonymous")
   (file :initarg :file :accessor file
         :type (or pathname null) :initform nil)
   (staff-set :initarg :staff-set :accessor staff-set)))

(define-association page-contains
    ((partition :type partition
                :multiplicity 1
                :kind :aggregation)
     (page :type page
           :multiplicity 0-*
           :ordered t)))

(defclass tempo (element)
  ((measure-duration :initarg :measure-duration :accessor measure-duration)))

(define-association partition-tempo
    ((partition :type partition
                :multiplicity 1
                :kind :aggregation)
     (tempo :type tempo
           :multiplicity 0-*
           :ordered t)))

(define-association tempo-measure
    ((tempo :type tempo
            :multiplicity 1)
     (measure :type measure
              :multiplicity 0-*
              :ordered t)))

(defclass horizontal-element (element)
  ())

(defclass staff (horizontal-element)
  ((pitch-range :initarg :pitch-range :reader pitch-range)))

(defclass ledger (horizontal-element)
  ())

(defclass clef (element)
  ())

(define-association staff-clef
    ((staff :type staff
            :multiplicity 1
            :kind :aggregation)
     (clef :type clef
           :multiplicity 1)))

(define-association line-contains
    ((line :type line
           :multiplicity 1
           :kind :aggregation)
     (band :type horizontal-element
           :multiplicity 1-*
           :ordered t)))

(define-association band-contains
    ((band :type horizontal-element
           :multiplicity 1)
     (note :type note
           :multiplicity 1-*)))


(defclass partition-parameters ()
  ((page-number-font    :initarg :page-number-font    :accessor page-number-font    :type string)
   (line-number-font    :initarg :line-number-font    :accessor line-number-font    :type string)
   (measure-number-font :initarg :measure-number-font :accessor measure-number-font :type string)
   (paper-size          :initarg :paper-size          :accessor paper-size          :type string)
   (staff-height        :initarg :staff-height        :accessor staff-height        :type real
                        :documentation "Unit: millimeter, values: 3, 5, 7 mm")))

(define-association configured
    ((partition :type partition
                :multiplicity 1
                :kind :aggregate)
     (parameters :type partition-parameters
                 :multiplicity 1)))




(defun create-partition (staff-set &key (title "untitled") (author "anonymous") parameters)
  (let ((partition  (make-instance 'partition :title title :author author :staff-set staff-set))
        (parameters (or parameters
                        (make-instance 'partition-parameters
                            :page-number-font "Helvetica-12"
                            :line-number-font "Helvetica-10"
                            :measure-number-font "Helvetica-8"
                            :paper-size "A4"
                            :orientation :portrait
                            :staff-height 5)))
        (page       (make-instance 'page :number 1))
        (line       (make-instance 'line :number 1))
        (measure    (make-instance 'measure :number 1))
        
        )
    parition))

(pprint (macroexpand-1 '(define-association configured
                         ((partition :type partition
                           :multiplicity 1
                           :kind :aggregate)
                          (parameters :type partition-parameters
                           :multiplicity 1)))))




;;;; THE END ;;;;
