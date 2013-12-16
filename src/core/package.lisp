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
   "*PAPERS*" "*STAVES/BASS*" "*STAVES/BASS-TREBBLE*"
   "*STAVES/BASS-TREBBLE15MA*" "*STAVES/BASS15MB-TREBBLE*"
   "*STAVES/BASS15MB-TREBBLE15MA*" "*STAVES/TREBBLE*" "ANNOTATION"
   "ANNOTATION-CONTAINS-P" "ANNOTATION-LINK" "ANNOTATION-UNLINK"
   "ASSOCIATEDP" "ATTACH" "AUTHOR" "BAND" "BAND-CONTAINS-CONTAINS-P"
   "BAND-CONTAINS-LINK" "BAND-CONTAINS-UNLINK" "BOTTOM-PITCH" "CLEF"
   "CLUSTER" "CONFIGURES-CONTAINS-P" "CONFIGURES-LINK"
   "CONFIGURES-UNLINK" "CREATE-BANDS" "CREATE-PARTITION" "DETACH"
   "DURATION" "DYNAMIC" "ELEMENT" "FILE" "FILENAME"
   "GIVES-PITCH-CONTAINS-P" "GIVES-PITCH-LINK" "GIVES-PITCH-UNLINK"
   "GIVES-TEMPO-CONTAINS-P" "GIVES-TEMPO-LINK" "GIVES-TEMPO-UNLINK"
   "GROUPS-CONTAINS-P" "GROUPS-LINK" "GROUPS-UNLINK" "IMAGE" "LEDGER"
   "LINE" "LINE-CONTAINS-HORIZONTALLY-CONTAINS-P"
   "LINE-CONTAINS-HORIZONTALLY-LINK" "LINE-CONTAINS-HORIZONTALLY-UNLINK"
   "LINE-CONTAINS-VERTICALLY-CONTAINS-P" "LINE-CONTAINS-VERTICALLY-LINK"
   "LINE-CONTAINS-VERTICALLY-UNLINK" "LINE-NUMBER-FONT" "MAXIMUM-PITCH"
   "MEASURE" "MEASURE-CONTAINS-CONTAINS-P" "MEASURE-CONTAINS-LINK"
   "MEASURE-CONTAINS-UNLINK" "MEASURE-DURATION" "MEASURE-NUMBER-FONT"
   "MINIMUM-PITCH" "NAME" "NOTE" "NUMBER" "NUMBERED" "PAGE"
   "PAGE-CONTAINS-CONTAINS-P" "PAGE-CONTAINS-LINK" "PAGE-CONTAINS-UNLINK"
   "PAGE-NUMBER-FONT" "PAPER-FORMAT" "PAPER-ORIENTATION"
   "PAPER-PRINTABLE-AREA" "PAPER-SIZE" "PAPER-SIZE-AND-PRINTABLE-AREA"
   "PARTITION" "PARTITION-CONTAINS-CONTAINS-P" "PARTITION-CONTAINS-LINK"
   "PARTITION-CONTAINS-UNLINK" "PARTITION-PARAMETERS"
   "PARTITION-TEMPO-CONTAINS-P" "PARTITION-TEMPO-LINK"
   "PARTITION-TEMPO-UNLINK" "PITCH" "STAFF" "STAFF-HEIGHT" "STAFF-SET"
   "START-TIME" "TEMPO" "TEXT" "TITLE" "TOP-PITCH"
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



(defpackage "ABNOTATION.MIDI"
  (:use "COMMON-LISP"
        "MIDI"
        "COM.INFORMATIMAGO.CLEXT.ASSOCIATION"
        "ABNOTATION.CORE")

  (:shadowing-import-from "ABNOTATION.CORE" "NUMBER")

  (:export
   ))


;;;; THE END ;;;;
