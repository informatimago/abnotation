;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               loader.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Loader file for the abnotation application.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-09-11 <PJB> Created.
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

;; (load "/home/pjb/works/gsharp/src/abnotation/loader.lisp")
(in-package "COMMON-LISP-USER")
(setf *print-right-margin* 110)

#+ccl (import 'ccl:getenv)
#+ccl (defsetf getenv ccl:setenv)

#+ccl (setf ccl:*default-external-format*           :unix
            ccl:*default-file-character-encoding*   :utf-8
            ccl:*default-line-termination*          :unix
            ccl:*default-socket-character-encoding* :utf-8)

(ql:quickload :com.informatimago.tools.pathname)


#+ccl (setf (logical-pathname-translations "CCL")
            (cons  (list "CCL:*.pathname-translations.*"
                         (merge-pathnames
                          (make-pathname :defaults (user-homedir-pathname)
                                         :directory '(:relative "LOGHOSTS")
                                         :name :wild
                                         :type :unspecific
                                         :version :wild)
                          (user-homedir-pathname)
                          nil))
                   (logical-pathname-translations "CCL")))

(load-logical-pathname-translations "ABNOTATION")

(defparameter *src*
  (com.informatimago.tools.pathname:translate-logical-pathname "ABNOTATION:SRC;"))
(let ((dir (truename (translate-logical-pathname "ABNOTATION:"))))
  #+ccl       (ccl::cd dir)
  #+clisp     (ext:cd  dir)
  #-(or ccl clisp) (error "(cd ~S)" dir))


(dolist (dir (find-asdf-subdirectories  (list *src*)))
  (unless (member "old" (pathname-directory dir) :test (function string=))
    (pushnew dir asdf:*central-registry* :test (function equalp))))

(ql:quickload :abnotation-cocoa)

