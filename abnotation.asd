;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               abnotation.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    ASD file to load the abnotation application.
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


#+clisp
(unless custom:*ansi*
  (warn "clisp should be used with -ansi or (setf custom:*ansi* t) in ~/.clisprc"))

(asdf:defsystem :abnotation

    ;; system attributes:
    
    :description  "ABNotation application"

    :long-description "

This application reads midi files and displays and prints partitions
according to the Alexis Bosch's synchrone musical notation.

"
    
    :author     "Pascal J. Bourguignon <pjb@informatimago.com>"
    
    :maintainer "Pascal J. Bourguignon <pjb@informatimago.com>"
    
    :licence "AGPL3"

    ;; component attributes:
    
    :name "abnotation"

    :version "1.0.0"

    :properties ((#:author-email                   . "pjb@informatimago.com")
                 (#:date                           . "Autumn 2013")
                 ((#:albert #:output-dir)          . "/tmp/documentation/abnotation/")
                 ((#:albert #:formats)             . ("docbook"))
                 ((#:albert #:docbook #:template)  . "book")
                 ((#:albert #:docbook #:bgcolor)   . "white")
                 ((#:albert #:docbook #:textcolor) . "black"))
    
    :depends-on (:gsharp :mcclim)
    #+asdf-unicode :encoding #+asdf-unicode :utf-8
    :components (
                 
                 (:file "clef" :pathname "src/clef"   :depends-on ())

                 ))

;;;; THE END ;;;;
