;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               view.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;     The abstract view of the ABNotation.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-12-13 <PJB> Created.
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


(defclass graphic-element ()
  ((box :initarg :box :accessor box :type rect)))
(defclass offsetable-box (graphic-element)
  ((offset :initarg :offset :accessor offset :type point)))
(defclass page-box (graphic-element)
  ())
(defclass measure-box (graphic-element)
  ((adjusted-width :initarg :adjusted-width :accessor adjusted-width :type coordinate)
   (front-kerning  :initarg :front-kerning  :accessor front-kerning  :type coordinate)))
(defclass line-box (offsetable-box)
  ())
(defclass ledger-box (offsetable-box)
  ())
(defclass staff-box (offsetable-box)
  ())
(defclass clef-box (offsetable-box)
  ())
(defclass sound-box (offsetable-box)
  ())
(defclass beam-box (offsetable-box)
  ())
(defclass dynamic-box (offsetable-box)
  ())
(defclass tenue-box (offsetable-box)
  ())
(defclass head-box (offsetable-box)
  ())
(defclass accidental-box (offsetable-box)
  ())
(defclass annotation-box (offsetable-box)
  ())



[load midi] --> (list (or note tempo)) --> [splice-measure] --> (list tempo->measure->sound)
(list tempo->measure->sound) --> [layout] --> (and boxes
                                                   page->line->measure->sound)


