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
;;;;    
;; (load "/home/pjb/works/gsharp/src/abnotation/loader.lisp")
(in-package "COMMON-LISP-USER")

(import 'ccl:getenv)
(defsetf getenv ccl:setenv)



#+ccl (setf ccl:*default-external-format*           :unix
            ccl:*default-file-character-encoding*   :utf-8
            ccl:*default-line-termination*          :unix
            ccl:*default-socket-character-encoding* :utf-8)

(setf *print-right-margin* 110)


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

;; #+ccl (translate-logical-pathname #P"ccl:PATCHWORK.pathname-translations.newest")
;; --> #P"/Users/pjb/LOGHOSTS/PATCHWORK"

(load-logical-pathname-translations "GSHARP")

(defparameter *src*
  #+ccl   #P"GSHARP:src;"
  #+clisp #P"GSHARP:SRC;")

#+ccl       (ccl::cd (truename #P"GSHARP:src;abnotation;"))
#+clisp     (ext:cd  (truename #P"GSHARP:SRC;ABNOTATION;"))
#-(or ccl clisp) (error "(cd #P\"GSHARP:SRC;ABNOTATION;\")")


(dolist (dir (find-asdf-subdirectories  (list *src*)))
  (unless (member "old" (pathname-directory dir) :test (function string=))
    (pushnew dir asdf:*central-registry* :test (function equalp))))

;; (ql:quickload :gsharp)
(ql:quickload :abnotation)

(load #+(or allegro ccl) #P"GSHARP:src;abnotation;gsharp-init.lisp"
      #-(or allegro ccl) #P"GSHARP:SRC;ABNOTATION;GSHARP-INIT.LISP")



(defun run-on-display (&optional (display (getenv "DISPLAY")))
  (setf (getenv "DISPLAY") display)
  (gsharp:gsharp :new-process t))

(defun gsharp-buffer ()
  (gsharp::buffer (first (gsharp::views (first gsharp::*gsharp-instances*)))))

(defmacro in-gsharp (&body body)
  `(let ((gsharp::*application-frame*  (first gsharp::*gsharp-instances*))
         (gsharp::*esa-instance*       (first gsharp::*gsharp-instances*)))
     (multiple-value-prog1 (progn ,@body)
       ;; (gsharp::redisplay-frame-panes (first gsharp::*gsharp-instances*) :force-p t)
       (let ((sheet (clim:frame-top-level-sheet (first gsharp::*gsharp-instances*))))
        (clim:repaint-sheet sheet (clim:sheet-region sheet))))))

