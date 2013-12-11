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


(defmacro defassoc (name &rest endpoints))

(defclass element ()
  ())

(defclass annotation (element)
  ())

(defclass image (annotation)
  ())

(defclass text (annotation)
  ())

(defassoc annotates
  (element :one owner)
  (annotation :optional annotation))


(defclass sound (element)
  ((start-time :initarg :start-time :accessor start-time )
   (duration  :initarg :duration :accessor duration)
   (dynamic :initarg :dynamic :accessor dynamic :initform nil)))

(defclass note (sound)
  ((pitch :initarg :pitch :accessor pitch)))


(defclass cluster (sound)
  ())

(defassoc groups
  (cluster :one group)
  (note :several notes))

(defclass numbered ()
  ((number :initarg :number :reader number)))

(defgeneric renumber (numbered))


(defclass measure (element numbered)
  ())

(defassoc contains
  (measure :one measure)
  (sound :several sounds))

(defclass line (element numbered)
  ())

(defassoc contains
  (line :one line)
  (measure :several measures))

(defclass page (element numbered)
  ())

(defassoc contains
  (page :one page)
  (line :several lines))

(defclass partition ()
  ())

(defassoc contains
  (partition :one partition)
  (page :several pages))

;;;; THE END ;;;;
