;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               package.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Packages of the ABNotation program.
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

(defpackage "COM.INFORMATIMAGO.OBJCL.WRAPPER"
  (:use "COMMON-LISP"
        ;; "CLOSER-MOP"
        "COM.INFORMATIMAGO.OBJCL"
        "COM.INFORMATIMAGO.CLEXT.CLOSER-WEAK"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.CIRCULAR"
        "COM.INFORMATIMAGO.COMMON-LISP.LISP-SEXP.SOURCE-FORM")
  #-(and)
  (:shadowing-import-from "CLOSER-MOP"
                          "STANDARD-CLASS" "STANDARD-METHOD" "STANDARD-GENERIC-FUNCTION"
                          "DEFMETHOD" "DEFGENERIC")
  (:import-from "COM.INFORMATIMAGO.OCLO"  "*NULL*" "NULLP")
  (:import-from "COM.INFORMATIMAGO.OBJCL" "YES" "NO")
  #+ccl
  (:import-from "CCL"
                "*LISP-CLEANUP-FUNCTIONS*"
                "*SAVE-EXIT-FUNCTIONS*"
                "*RESTORE-LISP-FUNCTIONS*"
                "DEF-LOAD-POINTERS"
                "*LISP-STARTUP-FUNCTIONS*")
  (:export

   "ON-QUIT" "ON-SAVE" "ON-RESTORE"
   "ON-APPLICATION-DID-FINISH-LAUNCHING" "ON-LOAD-AND-NOW"

   "*LISP-STARTUP-FUNCTIONS*"
   "*APPLICATION-DID-FINISH-LAUNCHING-FUNCTIONS*"
   "*SAVE-EXIT-FUNCTIONS*"
   "*RESTORE-LISP-FUNCTIONS*"
   "*LISP-USER-POINTER-FUNCTIONS*"
   "*LISP-CLEANUP-FUNCTIONS*"

   "MAP-NSARRAY" "DO-NSARRAY" "DO-NSDICTIONARY" "HANDLE"
   "UPDATE-HANDLE" "WRAPPER" "HANDLE" "WITH-HANDLE" "WRAP" "WRAPPING"
   "UNWRAPPING" "UNWRAP" "RELEASE" "ANONYMOUS-WRAPPER"
   "ANONYMOUS-WRAPPER-THUNK" "ANONYMOUS-WRAPPER-THUNK-SOURCE"
   "NSARRAY-TO-LIST" "LIST-TO-NSARRAY")

  (:export "TO-LISP" "TO-OBJC")
  
  (:export "ON-MAIN-THREAD"))


(defpackage "ABNOTATION.IO"
  (:use "COMMON-LISP")
  (:shadow "FORMAT")
  (:export "FORMAT"))


(defpackage "ABNOTATION.COMMAND"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.OBJCL"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY")
  (:shadowing-import-from "COM.INFORMATIMAGO.OBJCL.WRAPPER" "TO-LISP" "TO-OBJC")
  (:shadowing-import-from "ABNOTATION.IO" "FORMAT")
  (:export
   "*DEBUG-ON-ERROR*"
   "*MINIBUFFER*"
   "*MESSAGE*"
   "*CURRENT-VIEW*"
   "*CURRENT-WINDOW*"

   "CURRENT-VIEW"
   "CURRENT-WINDOW"
   
   "CALL-INTERACTIVELY"
   "COMMAND"
   "CREATE-KEYMAP"
   "CURRENT-KEYMAP"
   "CURRENT-VIEW"
   "DEFINE-KEY"
   "GET-KEYMAP"
   "GLOBAL-KEYMAP"
   "GLOBAL-SET-KEY"
   "INPUT-STATE"
   "INPUT-STATE-CURRENT-KEYMAP"
   "INPUT-STATE-MODAL-KEYMAP"
   "KBD"
   "KEY-BINDING"
   "KEY-SEQUENCE-STRING"
   "KEYMAP"
   "KEYMAP-BINDING"
   "KEYMAP-NAME"
   "KEYMAP-NAMED"
   "KEYMAP-PARENT"
   "KEYMAP-SET-KEY"
   "MESSAGE"
   "MODIFIER-STRING"
   "PROCESS-KEY"
   "PROCESS-KEY-EVENT")

  (:export
   "DEFCOMMAND"
   "SELECT-FILE"
   "ASK-USER"))

(defpackage "ABNOTATION.COCOA"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.OBJCL"
        "COM.INFORMATIMAGO.OBJCL.WRAPPER"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.CIRCULAR"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "ABNOTATION.SPAN"
        "ABNOTATION.CORE"
        "ABNOTATION.COMMAND")
  (:shadowing-import-from "ABNOTATION.SPAN" "TAILP")
  (:shadowing-import-from "ABNOTATION.CORE" "NUMBER")
  (:shadowing-import-from "ABNOTATION.IO"   "FORMAT")
  (:export "MAIN"))


;;;; THE END ;;;;
