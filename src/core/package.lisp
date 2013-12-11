;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               package.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Packages of the core of the ABNotation program.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-12-09 <PJB> Added this header.
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


(defpackage "ABNOTATION.CORE"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.CLEXT.ASSOCIATION")
  
  (:shadow "NUMBER")
  
  (:export
   "XOR")
  
  (:export
   "COORDINATE"
   "POINT" "POINT-P" "MAKE-POINT" "COPY-POINT"
   "POINT-X" "POINT-Y"
   "SIZE" "SIZE-P" "MAKE-SIZE" "COPY-SIZE"
   "SIZE-WIDTH" "SIZE-HEIGHT"
   "RECT" "RECT-P" "MAKE-RECT" "COPY-RECT" "RECT-ORIGIN" "RECT-SIZE" "RECT-TO-LIST"
   "RECT-X"  "RECT-Y"  "RECT-WIDTH"  "RECT-HEIGHT"
   "RECT-LEFT" "RECT-RIGHT" "RECT-BOTTOM" "RECT-TOP"
   "RANGE" "RANGE-P" "RANGE-LOCATION" "RANGE-LENGTH" "COPY-RANGE" "MAKE-RANGE")

  (:export
   "APPENDF" "NCONCF" "DELETEF" "ADD-TO-LIST"
   "DELETE-FROM-LIST" "INSERT-INTO-LIST" "DOVECTOR" "OBJECT-IDENTITY"
   "PRINT-PARSEABLE-OBJECT" "SIMPLE-PROGRAM-ERROR"
   "COPY-OBJECT-FROM"))


;;;; THE END ;;;;
