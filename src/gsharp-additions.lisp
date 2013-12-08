;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               gsharp-additions.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Define some additions to gsharp.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-12-08 <PJB> Created.
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

(in-package :gsharp)

(defmacro in-gsharp (&body body)
  `(let ((*application-frame*  (first *gsharp-instances*))
         (*esa-instance*       (first *gsharp-instances*)))
     (multiple-value-prog1 (progn ,@body)
       ;; (redisplay-frame-panes (first *gsharp-instances*) :force-p t)
       (let ((sheet (frame-top-level-sheet (first *gsharp-instances*))))
         (repaint-sheet sheet (sheet-region sheet))))))


(defgeneric buffer-name (object)
  (:documentation "Returns the name of the buffer.
OBJECT: a buffer or a view on a buffer.")
  (:method ((buffer gsharp-buffer:buffer))
    (esa-utils::name buffer))
  (:method ((view gsharp::orchestra-view))
    (buffer-name (buffer view))))


(defun buffer-list (&optional (*application-frame* *application-frame*))
  "Returns the list of buffers in the application."
  (mapcar (function buffer) (views *application-frame*)))



(defvar *default-staves-function* 'staves/treble15ma+treble+bass+bass15mb)
(defvar *default-minimum-width*   12)
(defvar *default-spacing-style*   1.0)
(defvar *default-right-edge*      700)
(defvar *default-left-offset*     30)
(defvar *default-left-margin*     20) 

(defun create-buffer (buffer-name)
  "Create a new gsharp buffer with the given name."
  (let* ((buffer   (make-instance 'buffer
                       :name            (unique-buffer-name buffer-name)
                       :min-width       *default-minimum-width*   
                       :spacing-style   *default-spacing-style*   
                       :right-edge      *default-right-edge*      
                       :left-offset     *default-left-offset*     
                       :left-margin     *default-left-margin*))
         (staves   (funcall *default-staves-function*))
         (layer    (make-layer  staves))
         (segments (list (make-instance 'segment
                             :buffer buffer
                             :layers (list layer)))))
    (setf (staves   buffer) staves
          (segments buffer) segments
          (needs-saving buffer) t)
    (add-buffer *application-frame* buffer)
    buffer))


(defun unique-buffer-name (basename)
  "Return a unique buffer name from the BASENAME."
  (let ((buffers (buffer-list)))
    (loop
      :for i :from 0
      :for name = basename :then (format nil "~A<~A>" basename i)
      :while (find name buffers :key (function buffer-name) :test (function string=))
      :finally (return name))))


(defgeneric create-file-buffer (filename)
  (:documentation "Create a new gsharp buffer for the file at the given pathname designator.")
  (:method ((filename string))
    (create-file-buffer (pathname filename)))
  (:method ((filename pathname))
    (let ((buffer (create-buffer (file-namestring filename))))
      (setf (filepath buffer) filename)
      buffer)))


(defgeneric get-buffer (buffer-or-name)
  (:documentation "Find a buffer with the given name.")
  (:method ((buffer buffer)) buffer)
  (:method ((name string))
    (find name (buffer-list) :key (function buffer-name) :test (function string=))))


(defgeneric get-buffer-create (buffer-or-name)
  (:documentation "Find or create a buffer with the given name.")
  (:method ((buffer buffer)) buffer)
  (:method ((name string))
    (or (get-buffer name) (create-buffer name))))



(defgeneric rename-buffer (buffer new-name &key unique)
  (:method ((old-name string) (new-name string) &key unique)
    (rename-buffer (or (get-buffer old-name)
                       (error "No buffer named ~S" old-name))
                   new-name :unique unique))
  (:method ((buffer buffer) (new-name string) &key unique)
    (unless (string= (buffer-name buffer) new-name)
      (let ((new-name (if unique
                          (unique-buffer-name new-name)
                          new-name)))
        (setf (esa-utils::name buffer) new-name)))
    buffer))



(import 'com.informatimago.common-lisp.cesarum.utility:compose)

(defun =list (&rest funs)
  (lambda (&rest args)
    (mapcar (lambda (fun)
              (apply fun args))
            funs)))

#-(and)
(progn
 (cl-user::in-gsharp
  (mapcar (=list (function buffer-name)
                 (compose filepath buffer))
          (views *application-frame*)))

 ;; (("abc.gsh" #P"/home/pjb/works/gsharp/src/abnotation/files/abc.gsh") ("*scratch*" nil))
 ;; 
 ;; ("abc.gsh" "*scratch*")


 (cl-user::in-gsharp (mapcar (function buffer-name) (buffer-list)))
 ;; ("*scratch*")
 (in-gsharp (unique-buffer-name "*scratchy*"))
 (in-gsharp (get-buffer-create "hello"))
 (in-gsharp (get-buffer-create "world"))
 (in-gsharp (buffer-list))
 (in-gsharp (view (car (windows *application-frame*))))
 (in-gsharp (rename-buffer (get-buffer "*scratch*") "*scratch*<4>"))
 (in-gsharp (views *application-frame*))
 (in-gsharp (mapcar 'buffer-name (buffer-list)))
 (in-gsharp (sort (mapcar 'buffer-name (buffer-list)) (function string<)))

 )
