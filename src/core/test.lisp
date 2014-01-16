;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               test.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;     Some test of the ABNotation core.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-01-11 <PJB> Created.
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
(in-package "ABNOTATION.CORE")


(defun test/make-measures (duration measure-duration)
  (let* ((tempo    (make-instance 'tempo :measure-duration measure-duration))
         (measures (loop
                     :for time :from 0.0 :to duration :by measure-duration
                     :for previous = nil :then measure
                     :for measure = (make-instance 'measure :start-time time)
                     :do (when previous (setf (next previous) measure))
                         (setf (previous measure) previous)
                         (attach 'gives-tempo tempo measure)
                     :collect measure)))
    measures))


(define-condition internal-error (simple-error)
  ((datum :initarg :datum :reader internal-error-datum)))


(defmethod add-measures-until (time (measure measure))
  (let ((tempo (tempo measure))))
  )


(defmethod insert-sound ((sound sound) (measure measure))
  (flet ((insert (sound measure)
           (attach 'measure-contains-sounds measure sound)))
    (let ((time (start-time sound)))
      (cond
        (cond
          ((< time (start-time measure))
           (let ((m (find-measure-containing-time time measure)))
             (if m
                 (insert sound m)
                 (error 'internal-error
                        :datum (list  :time time :measure measure)
                        :format-control "Cannot find a measure containing time ~S before ~S"
                        :format-arguments (list time measure)))))


          ((<= (end-time measure) time)
           (let ((m (find-measure-containing-time time measure)))
             (if m
                 (insert sound m)
                 (progn
                   (add-measures-until (end-time sound) measure)
                   (insert sound  (find-measure-containing-time time measure))))))
          (t
           (insert sound measure)))))))

(defun test/make-sounds ()
  (let ((m (test/make-measures 10.0 1.0))
        (s (append
            (loop
              :for p :from 22
              :for s :from 0.1 :to 2.9 :by 0.11
              :collect (make-instance 'note :pitch p :start-time s :duration 0.1))
            (loop
              :for p :from 22
              :for s :from 4.1 :to 6.9 :by 0.22
              :collect (make-instance 'note :pitch p :start-time s :duration 0.2))
            (loop
              :for p :from 22
              :for s :from 8.1 :to 9.9 :by 0.33
              :collect (make-instance 'note :pitch p :start-time s :duration 0.3)))))
    (insert-sound s (first m))
    ))

;; (find-measure-containing-time 0.4 (first (test/make-measures 5.0 1.0)))


;; (let ((m (test/make-measures 10.0 1.0)))
;;   (list (mapcar (function previous) m)
;;         m
;;         (mapcar (function next) m)))



(defun test/next/sound ()
  )
