;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               macro.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    ABNotation core macros.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-09 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2013
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

(in-package "ABNOTATION.CORE")

(define-modify-macro appendf (&rest args) 
  append "Append onto list")

(define-modify-macro nconcf (&rest args) 
  nconc "Nconc onto list")

(declaim (inline delete/swapped-arguments))
(defun delete/swapped-arguments (sequence item &rest keyword-arguments)
  (apply #'delete item sequence keyword-arguments))
(define-modify-macro deletef (item &rest remove-keywords)
  delete/swapped-arguments
  "Modify-macro for DELETE. Sets place designated by the first argument to
the result of calling DELETE with ITEM, place, and the REMOVE-KEYWORDS.")


(defmacro add-to-list (list-place element)
    "
DO:             Destructively add the ELEMENT to the LIST-PLACE in the
                last position.
"
  `(appendf ,list-place (list ,element)))


(defmacro delete-from-list (list-place element)
  "
DO:             Destructuvely delete from the list stored in place
                LIST-PLACE the ELEMENT.
"
  `(deletef ,list-place ,element))


(defmacro insert-into-list (&whole whole &environment env
                            list-place position element)
  "
DO:             Destructively insert into the LIST-PLACE the ELEMENT
                in the given position.

POSITION:       0 means insert in front of the list.
                n means after the n-th element.
"
  (multiple-value-bind (dummies vals new setter getter) (get-setf-expansion list-place env)
    (when (cdr new) (error "Can't expand ~S" whole))
    (let ((vposition (gensym))
          (velement  (gensym))
          (vplace    (car new)))
      `(let* (,@(mapcar #'list dummies vals) (,vplace ,getter)
                (,vposition ,position)
                (,velement  ,element))
         (if (zerop ,vposition)
             (push ,velement ,vplace)
             (push ,velement (cdr (or (nthcdr (1- ,vposition) ,vplace)
                                      (last ,vplace)))))
         ,setter))))


(defmacro dovector ((var vector &optional result) &body body)
  (let ((vvector (gensym "vector"))
        (vindex  (gensym "index"))
        (vlength (gensym "length")))
    `(block nil
       (let* ((,vvector ,vector)
              (,vlength (length ,vvector))
              (,vindex  -1))
         (tagbody
            (go :test)
          :loop
            (let ((,var (aref ,vvector ,vindex)))
              ,@body)
          :test
            (incf ,vindex)
            (if (< ,vindex ,vlength)
                (go :loop))
            (return ,result))))))



(define-condition simple-program-error (simple-error program-error)
  ())


(defgeneric copy-object-from (dst src)
  (:documentation "
DO:             Perform a deep copy of the slots of the SRC object to
                the DST object.  Methods are usually specialized  only
                for DST and SRC of the same class.

DST:            An instance.

SRC:            An instance.

RETURN:         DST
"))


(defmacro not-implemented-yet (&rest arguments)
  `(error "Not implemented yet ~S" ',arguments))

;;;; THE END ;;;;
