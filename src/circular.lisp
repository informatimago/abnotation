;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               circular.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Utility to deal with circular structures.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-04-14 <PJB> Created.
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

(in-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.CIRCULAR")

(defvar *circular-references* nil)

(defmacro with-circular-references ((&key (test ''eql)) &body body)
  `(let ((*circular-references* (cons (make-hash-table :test ,test) 0)))
     ,@body))

(defun circular-register (object)
  (let ((count (gethash object (car *circular-references*) 0)))
    (if count
      (= 1 (incf (gethash object (car *circular-references*) 0)))
      (progn
        (warn "BAD: re-registering ~S" object)
        nil))))

(defun circular-reference (object)
  (let ((index (gethash object (car *circular-references*))))
    (typecase index
      (null    nil)
      (integer (setf (gethash object (car *circular-references*))
                     (if (= 1 index)
                         nil
                         (cons (incf (cdr *circular-references*))
                               nil))))
      (t       index))))

;;;; THE END ;;;;
