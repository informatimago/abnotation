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

(defpackage "ABNOTATION.METAFONT"
  (:use "COMMON-LISP")
  (:export "MAKE-BEZIER-SEGMENT" "BEZIER-SEGMENT"
           "MAKE-OPEN-PATH" "MAKE-CLOSED-PATH"
           "CLOSED-PATH" "CONCATENATE-PATHS" "PATH-START"
           "CLOSE-PATH"
           "POLYGONALIZE"
           "PATH-BOUNDING-BOX"
           "SCAN-LINES"
           "FIRST-LINE" "NB-LINES" "CROSSINGS"
           "TRANSLATE" "ROTATE" "SCALE" "SLANT" "REVERSE-PATH"
           "DRAW-PATH" "WITH-PEN"
           "+RAZOR+" "+UNIT-SQUARE+"
           "+QUARTER-CIRCLE+" "+HALF-CIRCLE+" "+FULL-CIRCLE+"
           "SUPERELLIPSE"
           ;; MF-LIKE STUFF
           "PATHS" "MF" "PATHS" "CONTROL" "CONTROLS" "TENSION" "TENSIONS"
           "&" "--" "---" "CURL" "DIRECTION" "CYCLE"
           "LEFT" "RIGHT" "UP" "DOWN"))

(defpackage "ABNOTATION.SDL"
  (:use "COMMON-LISP"
        "ABNOTATION.METAFONT")
  (:export "GLYPH" "STAFF-LINE-DISTANCE" "STAFF-LINE-OFFSETS"
           "STEM-OFFSETS" "BAR-LINE-OFFSETS"
           "LEDGER-LINE-X-OFFSETS" "LEDGER-LINE-Y-OFFSETS"
           "NOTEHEAD-RIGHT-OFFSETS" "NOTEHEAD-LEFT-OFFSETS"
           "MAKE-FONT" "GLYPH-OFFSETS" "SUSPENDED-NOTE-OFFSET"
           "BEAM-OFFSETS" "BEAM-HANG-SIT-OFFSET"))


(defpackage "ABNOTATION.SPAN"
  (:use "COMMON-LISP")
  (:shadow "TAILP")
  (:export
   "HEAD" "TAIL" "NEXT" "PREVIOUS" "OWNER" "SPAN" "HEAD" "TAIL"
   "OWNER" "OWNED-SPAN" "OWNER" "NODE" "SPAN" "PREVIOUS" "NEXT"
   "PLACE-HOLDER-NODE" "INDIRECT-NODE" "ITEM"
   "MAKE-EMPTY-SPAN" "MAKE-SPAN" "EMPTYP"
   "HEADP" "TAILP" "HEAD-IN-SPAN-P" "TAIL-IN-SPAN-P"
   "PREVIOUS-SPAN" "NEXT-SPAN" "FIRST-SPAN-P" "LAST-SPAN-P"
   "FIRST-SPAN" "LAST-SPAN" "SPAN-LIST" "JOIN-SPANS"
   "SPAN-CONTENTS" "SPAN-NTH" "SPAN-LENGTH" "SPLIT-SPAN-BEFORE"
   "SPLIT-SPAN-AFTER" "FORWARD-SLURP-SPAN" "BACKWARD-SLURP-SPAN"
   "SPAN-PREPEND-NODE" "SPAN-APPEND-NODE" "EXTRACT-NODE"
   "INSERT-NODE-AFTER" "INSERT-NODE-BEFORE"
   "SPAN-POSITION-IF" "SPAN-POSITION"
   "FIND-NODE-IF" "DOSPAN" "MAPSPAN")
  
  (:documentation
   "

This package exports a double-linked list type.  This is a structure
optimized for insertions and deletions in any place, each node keeping
a pointer to both the previous and the next node.

The list is subdivided into a partition of contiguous spans. Each span
keeps a reference to its head and tail.


License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 2001 - 2013
    
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    
    This program is distributed in the hope that it will be useful, 
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.
    
    You should have received a copy of the GNU Affero General Public License
    along with this program.
    If not, see <http://www.gnu.org/licenses/>

"))


(defpackage "ABNOTATION.CORE"

  (:use "COMMON-LISP")
  (:shadow "NUMBER")

  (:use
   "MIDI"
   "COM.INFORMATIMAGO.CLEXT.ASSOCIATION"
   "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.QUEUE"
   "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY")

  (:use "ABNOTATION.SPAN")
  (:shadowing-import-from "ABNOTATION.SPAN" "TAILP")
  

  (:export ; macros
   "APPENDF" "NCONCF" "DELETEF" "ADD-TO-LIST"
   "DELETE-FROM-LIST" "INSERT-INTO-LIST" "DOVECTOR" 
   "SIMPLE-PROGRAM-ERROR"
   "COPY-OBJECT-FROM")

  (:export ; graphic
   "FILL-PATH" "STROKE-PATH" "DRAW-STRING" "SET-FONT" "SET-COLOR"
   "SET-STROKE-WIDTH")
  
  (:export ; geometry
   "ABOVE" "BELOW" "BOTTOM" "BOUNDS" "COORDINATE" "COPY-POINT"
   "COPY-RANGE" "COPY-RECT" "COPY-SIZE" "DEGREE" "DETERMINANT"
   "DOT-PRODUCT" "FRAME" "HEIGHT" "LEFT" "LEFT-OF" "LINE-INTERSECTION"
   "MAKE-POINT" "MAKE-RANGE" "MAKE-RECT" "MAKE-SIZE" "ORIGIN"
   "PILE-DOWN" "PLACE" "POINT" "POINT-P" "POINT-X" "POINT-Y" "POINT="
   "RADIAN" "RANGE" "RANGE-LENGTH" "RANGE-LOCATION" "RANGE-P" "RECT"
   "RECT-BOTTOM" "RECT-CENTER-X" "RECT-CENTER-Y" "RECT-EXPAND"
   "RECT-HEIGHT" "RECT-LEFT" "RECT-OFFSET" "RECT-ORIGIN" "RECT-P"
   "RECT-RIGHT" "RECT-SIZE" "RECT-TO-LIST" "RECT-TOP" "RECT-UNION"
   "RECT-WIDTH" "RECT-X" "RECT-X"  "RECT-Y"  "RECT-WIDTH" "RECT-Y"
   "RECT-INTERSECTION" "RECT-EMPTY-P" "LEFT-SIDE" "RIGHT-SIDE"
   "BOTTOM-SIDE" "TOP-SIDE" "EXTENT"
   "RIGHT" "RIGHT-OF" "ROTATE" "SIZE" "SIZE-HEIGHT" "SIZE-P"
   "SIZE-WIDTH" "SIZE=" "SQUARE" "STACK-OBJECTS" "STACK-UP" "TOP"
   "UNIT-VECTOR" "VECTOR*" "VECTOR+" "VECTOR-" "VECTOR-ABS"
   "VECTOR-ANGLE" "VECTOR-ROTATE" "VECTOR-X" "VECTOR-Y" "WIDTH")
  
  (:export ; bezier
   "*PATH-CLASS*" "ADD-ARC" "ADD-ARC-TO-POINT" "ADD-ELLIPSE-IN-RECT"
   "ADD-LINES" "ADD-PATH" "ADD-RECT" "ADD-RECTS" "BOUNDING-BOX"
   "CLOSE-SUBPATH" "COPY-PATH" "COPY-PATH-ELEMENT" "CREATE-PATH"
   "CURVE-TO-COORDINATES" "CURVE-TO-POINT" "LINE-TO-COORDINATES"
   "LINE-TO-POINT" "MOVE-TO-COORDINATES" "MOVE-TO-POINT" "PATH"
   "PATH-APPLY" "PATH-CONTAINS-POINT" "PATH-CURRENT-POINT" "PATH-ELEMENT"
   "PATH-ELEMENT-CONTROL-POINT" "PATH-ELEMENT-CONTROL-POINT-1"
   "PATH-ELEMENT-CONTROL-POINT-2" "PATH-ELEMENT-CURVE-TO-POINT"
   "PATH-ELEMENT-EQUAL" "PATH-ELEMENT-LINE-TO-POINT"
   "PATH-ELEMENT-MOVE-TO-POINT" "PATH-ELEMENT-POINT"
   "PATH-ELEMENT-QUAD-CURVE-TO-POINT" "PATH-EMPTY-P" "PATH-EQUAL"
   "PATH-RECT-P" "QUAD-CURVE-TO-COORDINATES" "QUAD-CURVE-TO-POINT"
   "SURROUNDING-BOX")

  (:export ; draw, graphic
   "DRAW" "SET-COLOR" "SET-FONT" "DRAW-STRING" "STROKE-PATH" "FILL-PATH"
   "DRAW-CLEF")
  
  (:export ; model
   "*PAPERS*" "*STAVES/BASS*" "*STAVES/BASS-TREBBLE*"
   "*STAVES/BASS-TREBBLE15MA*" "*STAVES/BASS15MB-TREBBLE*"
   "*STAVES/BASS15MB-TREBBLE15MA*" "*STAVES/TREBBLE*" "ANNOTATION"
   "ANNOTATION-CONTAINS-P" "ANNOTATION-LINK" "ANNOTATION-UNLINK"
   "ASSOCIATEDP" "ATTACH" "AUTHOR" "BAND" "BAND-CONTAINS"
   "BAND-CONTAINS-CONTAINS-P" "BAND-CONTAINS-LINK"
   "BAND-CONTAINS-UNLINK" "BANDS" "BOTTOM-PITCH" "CLEF" "CLUSTER"
   "CONFIGURES" "CONFIGURES-CONTAINS-P" "CONFIGURES-LINK"
   "CONFIGURES-UNLINK" "CREATE-BANDS" "CREATE-PARTITION" "DETACH"
   "DURATION" "DYNAMIC" "ELEMENT" "FILE" "FILENAME" "GIVES-PITCH"
   "GIVES-PITCH-CONTAINS-P" "GIVES-PITCH-LINK" "GIVES-PITCH-UNLINK"
   "GIVES-TEMPO" "GIVES-TEMPO-CONTAINS-P" "GIVES-TEMPO-LINK"
   "GIVES-TEMPO-UNLINK" "GROUPS" "GROUPS-CONTAINS-P" "GROUPS-LINK"
   "GROUPS-UNLINK" "IMAGE" "LEDGER" "LINE"
   "LINE-CONTAINS-HORIZONTALLY-CONTAINS-P"
   "LINE-CONTAINS-HORIZONTALLY-LINK"
   "LINE-CONTAINS-HORIZONTALLY-UNLINK" "LINE-CONTAINS-VERTICALLY"
   "LINE-CONTAINS-VERTICALLY-CONTAINS-P"
   "LINE-CONTAINS-VERTICALLY-LINK" "LINE-CONTAINS-VERTICALLY-UNLINK"
   "LINE-CONTAINS_HORIZONTALLY" "LINE-NUMBER-FONT" "LINES"
   "MAXIMUM-PITCH" "MEASURE" "MEASURE-CONTAINS"
   "MEASURE-CONTAINS-CONTAINS-P" "MEASURE-CONTAINS-LINK"
   "MEASURE-CONTAINS-UNLINK" "MEASURE-DURATION" "MEASURE-NUMBER-FONT"
   "MEASURES" "MINIMUM-PITCH" "NAME" "NOTE" "NOTES" "NUMBER"
   "NUMBER-ANNOTATION"
   "NUMBERED" "PAGE" "PAGE-CONTAINS" "PAGE-CONTAINS-CONTAINS-P"
   "PAGE-CONTAINS-LINK" "PAGE-CONTAINS-UNLINK" "PAGE-NUMBER-FONT"
   "PAGES" "PAPER-FORMAT" "PAPER-ORIENTATION" "PAPER-PRINTABLE-AREA"
   "PAPER-SIZE" "PAPER-SIZE-AND-PRINTABLE-AREA" "PARAMETERS"
   "PARTITION" "PARTITION-CONTAINS" "PARTITION-CONTAINS-CONTAINS-P"
   "PARTITION-CONTAINS-LINK" "PARTITION-CONTAINS-UNLINK"
   "PARTITION-PARAMETERS" "PARTITION-TEMPO"
   "PARTITION-TEMPO-CONTAINS-P" "PARTITION-TEMPO-LINK"
   "PARTITION-TEMPO-UNLINK" "PITCH" "SOUNDS" "STAFF" "STAFF-HEIGHT"
   "STAFF-SET" "START-TIME" "TEMPO" "TEMPOS" "TEXT" "TITLE" "RTF"
   "TOP-PITCH" 
   "NEEDS-SAVING"
   "BOX" "COMPUTE-BOX-SIZE" "BOX-SIZE" "OFFSET" "LAYOUT"
   "CURSOR" "ITEM")

  (:export ; midi stuff
   "MIDI-NOTE" "KEY" "INITIAL-VELOCITY" "CURRENT-VELOCITY"
   "NOTE-IS-ON" "NOTE-ON-TIME" "NOTE-OFF-TIME" "TIMINGS" "DURATION"
   "ABNOTATION-SEQUENCE" "NOTES" "READ-MIDI-TRACK"
   "APPEND-MIDI-SEQUENCE"
   "ABNOTATION-READ-MIDI-FILE")

  (:export
   "LAYOUT-PARTITION-FROM-TEMPOS"))



;;;; THE END ;;;;
