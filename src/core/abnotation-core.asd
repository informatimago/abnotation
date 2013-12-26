;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               abnotation.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    ASD file to load the core of the abnotation application.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-10-20 <PJB> Created this .asd file.
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
;;;;    along with this program.  If not, see http://www.gnu.org/licenses/
;;;;**************************************************************************

(asdf:defsystem "abnotation-core"

    ;; system attributes:
    
    :description  "The core of the ABNotation application"

    :long-description "

This application reads midi files and displays and prints partitions
according to the Alexis Bosch's synchrone musical notation.

"
    
    :author     "Pascal J. Bourguignon <pjb@informatimago.com>"
    
    :maintainer "Pascal J. Bourguignon <pjb@informatimago.com>"
    
    :licence "AGPL3"

    ;; component attributes:
    
    :name "abnotation"

    :version "0.0.0"

    :properties ((#:author-email                   . "pjb@informatimago.com")
                 (#:date                           . "Autumn 2013")
                 ((#:albert #:output-dir)          . "/tmp/documentation/abnotation/")
                 ((#:albert #:formats)             . ("docbook"))
                 ((#:albert #:docbook #:template)  . "book")
                 ((#:albert #:docbook #:bgcolor)   . "white")
                 ((#:albert #:docbook #:textcolor) . "black"))
    
    :depends-on ("midi"
                 "com.informatimago.clext"
                 "com.informatimago.common-lisp.cesarum")
    
    #+asdf-unicode :encoding #+asdf-unicode :utf-8
    :components
    ((:file "package"        :depends-on ())
     (:file "macros"         :depends-on ("package"))
     (:file "geometry"       :depends-on ("package" "macros"))
     (:file "matrix"         :depends-on ("package" "geometry"))
     (:file "transformation" :depends-on ("package" "geometry" "matrix"))
     (:file "bezier"         :depends-on ("package" "geometry" "matrix"))
     (:file "elliptical-arc" :depends-on ("package" "geometry" "matrix" "bezier"))
     (:file "graphic"        :depends-on ("package" "bezier" "elliptical-arc"))
     (:file "model"          :depends-on ("package" "macros"))
     (:file "midi"           :depends-on ("package" "macros" "model"))
     (:file "layout"         :depends-on ("package" "macros" "model" "bezier"))
     (:file "draw"           :depends-on ("package" "macros" "model" "bezier" "layout"
                                          "graphic"))

     ))

;;;; THE END ;;;;
