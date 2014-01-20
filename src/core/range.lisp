;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               range.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    XXX
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-01-18 <PJB> 
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2014 - 2014
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
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************

(defpackage "ABNOTATION.RANGE"
  (:use "COMMON-LISP")
  (:shadow "TAILP")
  (:export
   "HEAD" "TAIL" "NEXT" "PREVIOUS" "OWNER" "RANGE" "HEAD" "TAIL"
   "OWNER" "OWNED-RANGE" "OWNER" "NODE" "RANGE" "PREVIOUS" "NEXT"
   "PLACE-HOLDER-NODE" "MAKE-EMPTY-RANGE" "MAKE-RANGE" "EMPTYP"
   "HEADP" "TAILP" "HEAD-IN-RANGE-P" "TAIL-IN-RANGE-P"
   "RANGE-CONTENTS" "RANGE-NTH" "RANGE-LENGTH" "SPLIT-RANGE-BEFORE"
   "SPLIT-RANGE-AFTER" "FORWARD-SLURP-RANGE" "BACKWARD-SLURP-RANGE"
   "RANGE-PREPEND-NODE" "RANGE-APPEND-NODE" "REMOVE-NODE"
   "INSERT-NODE-AFTER" "INSERT-NODE-BEFORE"
   "RANGE-POSITION-IF" "RANGE-POSITION"
   "FIND-NODE-IF" )
  
  (:documentation
   "

This module exports a double-linked list type.  This is a structure
optimized for insertions and deletions in any place, each node keeping
a pointer to both the previous and the next node.  Furthermore, ranges keep
references to the head list.


License:

    AGPL3
    
    Copyright Pascal J. Bourguignon 2001 - 2013
    
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    
    This program is distributed in the hope that it will be useful, 
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.
    
    You should have received a copy of the GNU Affero General Public License
    along with this program.
    If not, see <http://www.gnu.org/licenses/>

"))
(in-package "ABNOTATION.RANGE")


;;; ordered sequence
;;; - insert, remove, append.
;;; - ranges associated with an upper container.
;;; - several kinds of ranges (measures -> tempo, measures -> line).;
;;; item -> range -> first/last item

;; (insert-element (pages partition) page 0)
;; (insert-element (pages partition) page following)
;; (insert-element (pages partition) page :last)
;; 
;; (remove-element (pages partition) 42)
;; (remove-element (pages partition) page)
;; (remove-element (pages partition) :last)
;; 
;; (add-element (pages partition) page)


;; com.informatimago.common-lisp.cesarum.dll:dll


 ;;         ____________            _          ranges
 ;;        /            \           |
 ;;       []============[]==========[]        dll objects
 ;;       |             |           |           
 ;;   ____|_____      __|___     ___|__        ranges
 ;;  /  |   |   \    /  |   \   /   |  \
 ;; []==[]==[]==[]==[]==[]==[]==[]==[]==[]     dll objects



(defgeneric head (item))
(defgeneric tail (item))
(defgeneric next (item))
(defgeneric previous (item))
(defgeneric owner (item)
  (:method ((item t)) nil))


(defclass range ()
  ((head :initarg :head :initform nil :accessor head)
   (tail :initarg :tail :initform nil :accessor tail)))

(defmethod owner ((item range))
  item)

(defclass owned-range (range)
  ((owner :initarg :owner :initform nil :accessor owner)))

(defclass node ()
  ((range    :initarg :range  :initform nil :accessor range)
   (previous :initarg :prev   :initform nil :accessor previous)
   (next     :initarg :next   :initform nil :accessor next)))

(defclass place-holder-node (node)
  ())



(defun make-empty-range ()
  (let ((place-holder (make-instance 'place-holder-node)))
    (make-instance 'range :head place-holder :tail place-holder)))

(defun make-range (nodes)
  (if nodes
      (loop
        :with range = (make-instance 'range)
        :for previous = nil :then (car current)
        :for current :on nodes
        :initially (setf (head range) (car current))
        :do (setf (range (car current)) range)
            (setf (previous (car current)) previous
                  (next (car current)) (cadr current))
        :finally (setf (tail range) previous)
                 (return range))
      (make-empty-range)))


(defgeneric emptyp (range)
  (:method ((range range))
    (typep (head range) 'place-holder-node)))

(defgeneric headp (node)
  (:method ((node node))
    (null (previous node))))

(defgeneric tailp (node)
  (:method ((node node))
    (null (next node))))

(defgeneric head-in-range-p (node)
  (:method ((node node))
    (eql node (head (range node)))))

(defgeneric tail-in-range-p (node)
  (:method ((node node))
    (eql node (tail (range node)))))


(defgeneric range-contents (range)
  (:method ((range range))
    (unless (emptyp range)
      (loop
        :for current = (head range) :then (next current)
        :collect current
        :until (eql current (tail range))))))

(defgeneric range-nth (index range)
  (:method (index (range range))
    (when (and (<= 0 index) (head range))
      (loop
        :for node = (head range) :then (next node)
        :do (assert node)
        :until (or (zerop index) (eql node (tail range)))
        :do (decf index)
        :finally (return (when (zerop index)
                           node))))))

(defgeneric range-length (range)
  (:method ((range range))
    (if (emptyp range)
        0
        (loop
          :for node = (head range) :then (next node)
          :do (assert node)
          :count 1
          :until (eql node (tail range))))))



(defgeneric %update-node-range (range)
  (:method ((range range))
    (loop
      :for current = (head range) :then (next current)
      :do (setf (range current) range)
      :until (eql current (tail range)))))


(defgeneric split-range-before (range node-or-index)
  (:method ((range range) (index integer))
    (split-range-before range (range-nth index range)))
  (:method ((range range) (node node))
    (assert (eql range (range node)))
    (if (head-in-range-p node)
        (let* ((new-range (make-empty-range))
               (place-holder (tail new-range)))
          (%insert-node-before place-holder (head range))
          (values new-range range))
        (let* ((new-head (head range))
               (new-tail (previous node))
               (new-range (make-instance 'range :head new-head :tail new-tail)))
          (%update-node-range new-range)
          (setf (head range) node)
          (values new-range range)))))

(defgeneric split-range-after (range node-or-index)
  (:method ((range range) (index integer))
    (split-range-after range (range-nth index range)))
  (:method ((range range) (node node))
    (assert (eql range (range node)))
    (if (tail-in-range-p node)
        (let* ((new-range (make-empty-range))
               (place-holder (tail new-range)))
          (%insert-node-after place-holder (tail range))
          (values range new-range))
        (let* ((new-head (next node))
               (new-tail (tail range))
               (new-range (make-instance 'range :head new-head :tail new-tail)))
          (%update-node-range new-range)
          (setf (tail range) node)
          (values range new-range)))))

(defgeneric %delete-node (node)
  (:method ((node node))
    (when (next node)
      (setf (previous (next node)) (previous node)))
    (when (previous node)
      (setf (next (previous node)) (next node)))
    (setf (previous node) nil
          (next node) nil)
    node))

(defgeneric %insert-node-before (node next)
  (:method ((node node) (next node))
    (setf (previous node) (previous next))
    (when (previous next)
      (setf (next (previous next)) node))
    (setf (previous next) node
          (next node) next)
    node))

(defgeneric %insert-node-after (node previous)
  (:method ((node node) (previous node))
    (setf (next node) (next previous))
    (when (next previous)
      (setf (previous (next previous)) node))
    (setf (next previous) node
          (previous node) previous)
    node))

(defgeneric forward-slurp-range (range)
  (:method ((range range))
    (let* ((tail (tail range))
           (head (next tail))
           (next-range (and head (range head))))
      (when (and next-range (not (emptyp next-range)))
        (if (emptyp range)
            (let ((next-tail (tail next-range)))
              (%insert-node-after (%delete-node tail) next-tail)
              (setf (head range) head
                    (tail range) next-tail
                    (head next-range) tail
                    (tail next-range) tail))
            (let ((empty-node (make-instance 'place-holder-node))
                  (next-tail (tail next-range)))
              (%insert-node-after empty-node next-tail) 
              (setf (tail range) next-tail
                    (head next-range) empty-node
                    (tail next-range) empty-node)))
        (%update-node-range range)
        (%update-node-range next-range))
      range)))

(defgeneric backward-slurp-range (range)
  (:method ((range range))
    (let* ((head (head range))
           (tail (previous head))
           (previous-range (and tail (range tail))))
      (when (and previous-range (not (emptyp previous-range)))
        (if (emptyp range)
            (let ((previous-head (head previous-range)))
              (%insert-node-before (%delete-node head) previous-head)
              (setf (head range) previous-head
                    (tail range) tail
                    (head previous-range) head
                    (tail previous-range) head))
            (let ((empty-node (make-instance 'place-holder-node))
                  (previous-head (head previous-range)))
              (%insert-node-before empty-node previous-head) 
              (setf (head range) previous-head
                    (head previous-range) empty-node
                    (tail previous-range) empty-node)))
        (%update-node-range range)
        (%update-node-range previous-range))
      range)))


(defgeneric range-prepend-node (range node)
  (:method ((range range) (node node))
    (setf (range node) range)
    (if (emptyp range)
        (setf (previous node) (previous (head range))
              (next node) (next (tail range))
              (head range) node
              (tail range) node)
        (setf (previous node) (previous (head range))
              (previous (head range)) node
              (next node) (head range)
              (head range) node))))

(defgeneric range-append-node (range node)
  (:method ((range range) (node node))
    (setf (range node) range)
    (if (emptyp range)
        (setf (previous node) (previous (head range))
              (next node) (next (tail range))
              (head range) node
              (tail range) node)
        (setf (next node) (next (tail range))
              (next (tail range)) node
              (previous node) (tail range)
              (tail range) node))))

(defgeneric remove-node (node)
  (:method ((node node))
    (let ((range (range node)))
      (when (next node)
        (setf (previous (next node)) (previous node)))
      (when (previous node)
        (setf (next (previous node)) (next node)))
      (if (eql (head range) (tail range))
          (let ((place-holder (make-instance 'place-holder-node
                                             :previous (previous (head range))
                                             :next (next (tail range)))))
            (setf (head range) place-holder
                  (tail range) place-holder))
          (progn
            (when (eql node (head range))
              (setf (head range) (next node)))
            (when (eql node (tail range))
              (setf (tail range) (previous node)))))
      (setf (range node) nil)
      node)))

(defgeneric insert-node-after (node previous)
  (:method ((node node) (previous node))
    (setf (range node) (range previous))
    (%insert-node-after node previous)
    (when (eql previous (tail (range previous)))
      (setf (tail (range previous)) node))
    node))

(defgeneric insert-node-before (node next)
  (:method ((node node) (next node))
    (setf (range node) (range next))
    (%insert-node-before node next)
    (when (eql next (head (range previous)))
      (setf (head (range next)) node))
    node))


(defgeneric find-node-if (predicate head)
  (:method (predicate (head node))
    (loop
      :for node = head :then (next node)
      :while node
      :when (funcall predicate node)
        :do (return-from find-node-if node))))


(defgeneric range-position-if (predicate range &key key start end)
  (:method (predicate (range range) &key (key (function identity)) (start 0) end)
    (when (head range)
      (loop
        :for index :from 0
        :for node = (head range) :then (next node)
        :do
           (assert node)
           (cond
             ((and (<= start index)
                   (or (null end) (< index end))
                   (funcall predicate (funcall key node)))
              (return-from range-position-if index))
             ((eql node (tail range))
              (return-from range-position-if nil)))))))


(defgeneric range-position (node range &key test key start end)
  (:method ((node node) (range range) &key (test (function eql)) (key (function identity)) (start 0) end)
    (let ((value (funcall key node)))
      (range-position-if (lambda (node) (funcall test value (funcall key node)))
                         range :key key :start start :end end))))





;; measure -> tempo
;;      t1            t2
;; -------------- -----------
;; No.  


;; measure -> line
;; m1 m2 m3 m4 m5 m6 m7 m8 m9
;; -------- -------- --------
;;   l1        l2       l3






(defun first-segment-p (segment)
  (let ((measure (measure segment))
        (sound (sound segment)))
    (time-in-measure-p (start-time sound) measure)))

(defun intermediate-segment-p (segment)
  (let ((measure (measure segment))
        (sound (sound segment)))
    (and (not (time-in-measure-p (start-time sound) measure))
         (not (time-in-measure-p (end-time   sound) measure)))))

(defun last-segment-p (segment)
  (let ((measure (measure segment))
        (sound (sound segment)))
    (time-in-measure-p (end-time sound) measure)))


(defun first-element-in-container-p (element)
  (not (eql (container (previous element))
            (container element))))

(defun last-element-in-container-p (element)
  (not (eql (container element)
            (container (next element)))))




(defclass partition (element)
  ((pages  :type owned-range)
   (tempos :type owned-range)))

(defclass line (graphic-elment numbered node range)
  ())

(defclass tempo (elment node range)
  ())

(defclass measure (graphic-elment numbered node range)
  ())




;; (setf lisp-indent-function 'common-lisp-indent-function) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests
;;;


(defclass test/node (node)
  ((label :initarg :label :reader label :initform (gensym))))

(defmethod print-object ((node test/node) stream)
  (print-unreadable-object (node stream :identity nil :type t)
    (format stream "~A" (label node)))
  node)


(defun test/ranges ()
  (let ((empty (make-empty-range)))
    (assert (emptyp empty))
    (assert (zerop (range-length empty)))
    (assert (equal '() (range-contents empty))))
  (let* ((nodes (list (make-instance 'test/node)
                      (make-instance 'test/node)
                      (make-instance 'test/node)
                      (make-instance 'test/node)
                      (make-instance 'test/node)))
         (range (make-range nodes)))
    (assert (= (length nodes) (range-length range)))
    (assert (equal nodes (range-contents range)))
    (assert (headp (first nodes)))
    (assert (tailp (first (last nodes))))
    (assert (head-in-range-p (first nodes)))
    (assert (tail-in-range-p (first (last nodes))))
    (assert (every (lambda (node) (eql range (range node))) (range-contents range)))
    (loop
      :for node :in nodes
      :for index :from 0
      :do (assert (eql node (range-nth index range)))))
  :success)


(defun test/split-ranges ()
  (loop
    :for i :from 0 :to 4
    :do (let* ((nodes (list (make-instance 'test/node)
                            (make-instance 'test/node)
                            (make-instance 'test/node)
                            (make-instance 'test/node)
                            (make-instance 'test/node)))
               (range (make-range nodes)))
          (multiple-value-bind (a b) (split-range-before range i)
            (assert (= i (range-length a)))
            (assert (equalp nodes (append (range-contents a) (range-contents b))))
            (assert (every (lambda (node) (eql a (range node))) (range-contents a)))
            (assert (every (lambda (node) (eql b (range node))) (range-contents b)))))
    :do (let* ((nodes (list (make-instance 'test/node)
                            (make-instance 'test/node)
                            (make-instance 'test/node)
                            (make-instance 'test/node)
                            (make-instance 'test/node)))
               (range (make-range nodes)))
          (multiple-value-bind (a b) (split-range-after range i)
            (assert (= (1+ i) (range-length a)))
            (assert (equalp nodes (append (range-contents a) (range-contents b))))
            (assert (every (lambda (node) (eql a (range node))) (range-contents a)))
            (assert (every (lambda (node) (eql b (range node))) (range-contents b))))))
  :success)


(defun test/range-append-node ()
  (let* ((nodes (list (make-instance 'test/node)
                      (make-instance 'test/node)
                      (make-instance 'test/node)
                      (make-instance 'test/node)
                      (make-instance 'test/node)))
         (range (make-empty-range)))
    (loop
      :for node :in nodes
      :do (range-append-node range node))
    (assert (equal nodes (range-contents range))))
  :success)


(defun test/range-prepend-node ()
  (let* ((nodes (list (make-instance 'test/node)
                      (make-instance 'test/node)
                      (make-instance 'test/node)
                      (make-instance 'test/node)
                      (make-instance 'test/node)))
         (range (make-empty-range)))
    (loop
      :for node :in nodes
      :do (range-prepend-node range node))
    (assert (equal (reverse nodes) (range-contents range))))
  :success)


(defun test/forward-slurp-range ()
  (let* ((nodes (list (make-instance 'test/node)
                      (make-instance 'test/node)
                      (make-instance 'test/node)
                      (make-instance 'test/node)
                      (make-instance 'test/node)
                      (make-instance 'test/node)
                      (make-instance 'test/node)
                      (make-instance 'test/node)
                      (make-instance 'test/node)
                      (make-instance 'test/node)
                      (make-instance 'test/node))))
    (loop
      :for i :from 0 :to (- (length nodes) 4)
      :do (let ((r1 (make-range nodes))
                r2 r3 r4)
            (multiple-value-setq (r1 r2) (split-range-before r1 2))
            (multiple-value-setq (r2 r3) (split-range-before r2 i))
            (multiple-value-setq (r3 r4) (split-range-before r3 (- (length nodes) 4 i)))
            #-(and)(progn (print (list (range-length r1)
                                       (range-length r2)
                                       (range-length r3)
                                       (range-length r4)))
                          (print (list (range-contents r1)
                                       (range-contents r2)
                                       (range-contents r3)
                                       (range-contents r4)))
                          (print '-->))
            (assert (equal nodes (append (range-contents r1)
                                         (range-contents r2)
                                         (range-contents r3)
                                         (range-contents r4))))
            (forward-slurp-range r2)
            #-(and) (progn (print (list (range-length r1)
                                        (range-length r2)
                                        (range-length r3)
                                        (range-length r4)))
                           (print (list (range-contents r1)
                                        (range-contents r2)
                                        (range-contents r3)
                                        (range-contents r4))))
            (assert (emptyp r3))
            (assert (= 2 (range-length r1)))
            (assert (= 2 (range-length r4)))
            (assert (equal nodes (append (range-contents r1)
                                         (range-contents r2)
                                         (range-contents r4)))))))
  :success)

(defun test/backward-slurp-range ()
  (let* ((nodes (list (make-instance 'test/node)
                      (make-instance 'test/node)
                      (make-instance 'test/node)
                      (make-instance 'test/node)
                      (make-instance 'test/node)
                      (make-instance 'test/node)
                      (make-instance 'test/node)
                      (make-instance 'test/node)
                      (make-instance 'test/node)
                      (make-instance 'test/node)
                      (make-instance 'test/node))))
    (loop
      :for i :from 0 :to (- (length nodes) 4)
      :do (let ((r1 (make-range nodes))
                r2 r3 r4)
            (multiple-value-setq (r1 r2) (split-range-before r1 2))
            (multiple-value-setq (r2 r3) (split-range-before r2 i))
            (multiple-value-setq (r3 r4) (split-range-before r3 (- (length nodes) 4 i)))
            #-(and) (progn (print (list (range-length r1)
                                        (range-length r2)
                                        (range-length r3)
                                        (range-length r4)))
                           (print (list (range-contents r1)
                                        (range-contents r2)
                                        (range-contents r3)
                                        (range-contents r4)))
                           (print '-->))
            (assert (equal nodes (append (range-contents r1)
                                         (range-contents r2)
                                         (range-contents r3)
                                         (range-contents r4))))
            (backward-slurp-range r3)
            #-(and) (progn (print (list (range-length r1)
                                        (range-length r2)
                                        (range-length r3)
                                        (range-length r4)))
                           (print (list (range-contents r1)
                                        (range-contents r2)
                                        (range-contents r3)
                                        (range-contents r4))))
            (assert (emptyp r2))
            (assert (= 2 (range-length r1)))
            (assert (= 2 (range-length r4)))
            (assert (equal nodes (append (range-contents r1)
                                         (range-contents r3)
                                         (range-contents r4)))))))
  :success)



(defun check-node-dll (node)
  (let* ((head           (loop
                           :for current = node :then (previous current)
                           :while (previous current)
                           :do (assert (eql (next (previous current)) current))
                           :finally (return current)))
         (tail           (loop
                           :for current = node :then (next current)
                           :while (next current)
                           :do (assert (eql (previous (next current)) current))
                           :finally (return current)))
         (forward-nodes  (loop :for current = head :then (next current)
                               :collect current
                               :while (next current)))
         (backward-nodes (loop :for current = tail :then (previous current)
                               :collect current
                               :while (previous current))))
    (assert (equal (reverse backward-nodes) forward-nodes))
    (assert (member node forward-nodes))
    :success))

(defun check-range (range)
  (check-node-dll (head range))
  (check-node-dll (tail range))
  :success)

(defmethod dump ((range range))
  (print 'forward)
  (loop :for current = (head range) :then (next current)
        :do (print current)
        :while (next current))
  (print 'backward)
  (loop :for current = (tail range) :then (previous current)
        :do (print current)
        :while (previous current)))

(progn
  (test/ranges)
  (test/split-ranges)
  (test/range-append-node)
  (test/range-prepend-node)
  (test/forward-slurp-range)
  (test/backward-slurp-range))



#-(and) (progn
          (let ((range (make-empty-range))
                (n1 (make-instance 'test/node))
                (n2 (make-instance 'test/node))
                (n3 (make-instance 'test/node)))
            (range-append-node range n1)
            (check-range range)
            (range-contents range)
            (range-append-node range n2)
            (check-range range)
            (range-contents range)
            (range-append-node range n3)
            (check-range range)
            (range-contents range))

          (let ((range (make-empty-range))
                (n1 (make-instance 'test/node))
                (n2 (make-instance 'test/node))
                (n3 (make-instance 'test/node)))
            (range-append-node range n1)
            (check-range range)
            (range-contents range)
            (range-append-node range n2)
            (check-range range)
            (range-contents range)
            (range-append-node range n3)
            (check-range range)
            (values (range-contents range)
                    (list n1 n2 n3)))

          (let ((range (make-empty-range))
                (n1 (make-instance 'test/node))
                (n2 (make-instance 'test/node))
                (n3 (make-instance 'test/node)))
            (range-prepend-node range n1)
            (check-range range)
            (range-contents range)
            (range-prepend-node range n2)
            (check-range range)
            (range-contents range)
            (range-prepend-node range n3)
            (check-range range)
            (values (range-contents range)
                    (list n1 n2 n3)))


          (progn
            (print (range-contents range))
            
            (print (range-contents range))
            (range-append-node range n3)
            (print (range-contents range))))



;;;; THE END ;;;;
