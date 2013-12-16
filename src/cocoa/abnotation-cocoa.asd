;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               abnotation-cocoa.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    ASD file to load the abnotation-cocoa application.
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

#+ccl (require :cocoa)

(asdf:defsystem "abnotation-cocoa"

    ;; system attributes:
    
    :description  "ABNotation Cocoa application"

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
    
    :depends-on ("com.informatimago.objcl"
                 "com.informatimago.clext"
                 "com.informatimago.common-lisp.cesarum"
                 "abnotation-core")
    
    #+asdf-unicode :encoding #+asdf-unicode :utf-8
    :components (
                 (:file "package")
                 (:file "macros"     :depends-on ("package"))
                 (:file "system"     :depends-on ("package"))
                 (:file "wrapper"    :depends-on ("package" "macros" "system"))
                 (:file "abgeometry" :depends-on ("package" "wrapper"))
                 (:file "bezier"     :depends-on ("package" "wrapper" "abgeometry"))
                 (:file "keymap"     :depends-on ("package"))
                 (:file "views"      :depends-on ("package" "keymap"))
                 (:file "abview"     :depends-on ("package" "keymap" "abgeometry"))
                 (:file "abwindow"   :depends-on ("package" "keymap" "views" "macros"
                                                            "abgeometry"  "abview"))

                 (:file "main"       :depends-on ("package" "macros" "abgeometry" "abview"
                                                            "abwindow"))
                 ))

;;;; THE END ;;;;
