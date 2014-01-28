;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               span.lisp
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


(in-package "ABNOTATION.SPAN")


;;; ordered sequence
;;; - insert, remove, append.
;;; - spans associated with an upper container.
;;; - several kinds of spans (measures -> tempo, measures -> line).;
;;; item -> span -> first/last item

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


 ;;         ____________            _          spans
 ;;        /            \           |
 ;;       []============[]==========[]        dll objects
 ;;       |             |           |           
 ;;   ____|_____      __|___     ___|__        spans
 ;;  /  |   |   \    /  |   \   /   |  \
 ;; []==[]==[]==[]==[]==[]==[]==[]==[]==[]     dll objects



(defgeneric head (item))
(defgeneric tail (item))
(defgeneric next (item))
(defgeneric previous (item))
(defgeneric owner (item)
  (:method ((item t)) nil))


(defclass span ()
  ((head :initarg :head :initform nil :accessor head)
   (tail :initarg :tail :initform nil :accessor tail)))

(defmethod owner ((item span))
  item)

(defclass owned-span (span)
  ((owner :initarg :owner :initform nil :accessor owner)))

(defclass node ()
  ((span     :initarg :span     :initform nil :accessor span)
   (previous :initarg :previous :initform nil :accessor previous)
   (next     :initarg :next     :initform nil :accessor next)))

(defclass place-holder-node (node)
  ())

(defclass indirect-node (node)
  ((item     :initarg :item     :initform nil :accessor item)))

(defmethod item ((node place-holder-node))
  nil)

(defmethod item ((node node))
  node)


(defmethod initialize-instance :after ((span span) &key &allow-other-keys)
  (unless (and (slot-boundp span 'head) (slot-value span 'head))
    (let ((place-holder (make-instance 'place-holder-node)))
      (setf (head span) place-holder
            (tail span) place-holder)
      (%update-node-span span))))

(defun make-empty-span ()
  (make-instance 'span))

(defun make-span (nodes)
  (if nodes
      (loop
        :with span = (make-instance 'span)
        :for previous = nil :then (car current)
        :for current :on nodes
        :initially (setf (head span) (car current))
        :do (setf (span (car current)) span)
            (setf (previous (car current)) previous
                  (next (car current)) (cadr current))
        :finally (setf (tail span) previous)
                 (return span))
      (make-empty-span)))

(defmacro dospan ((variable span &optional result) &body body)
  (let ((vcurrent (gensym)))
    `(do ((,vcurrent (head ,span) (next ,vcurrent)))
         ((progn
            (unless (typep ,vcurrent 'place-holder-node)
              (let ((,variable ,vcurrent))
                ,@body))
            (tail-in-span-p ,vcurrent))
          ,result))))


(defgeneric emptyp (span)
  (:method ((span span))
    (typep (head span) 'place-holder-node)))

(defgeneric headp (node)
  (:method ((node node))
    (null (previous node))))

(defgeneric tailp (node)
  (:method ((node node))
    (null (next node))))


(defgeneric head-in-span-p (node)
  (:method ((node node))
    (eql node (head (span node)))))

(defgeneric tail-in-span-p (node)
  (:method ((node node))
    (eql node (tail (span node)))))


(defgeneric previous-span (span)
  (:method ((span span))
    (let ((previous (previous (head span))))
      (when previous
        (span previous)))))

(defgeneric next-span (span)
  (:method ((span span))
    (let ((next (next (tail span))))
      (when next
        (span next)))))

(defgeneric first-span-p (span)
  (:method ((span span))
    (not (previous-span span))))

(defgeneric last-span-p (span)
  (:method ((span span))
    (not (next-span span))))

(defgeneric first-span (span)
  (:method ((span span))
    (loop
      :until (first-span-p span)
      :do (setf span (previous-span span))
      :finally (return span))))

(defgeneric last-span (span)
  (:method ((span span))
    (loop
      :until (last-span-p span)
      :do (setf span (next-span span))
      :finally (return span))))

(defgeneric span-list (span)
  (:documentation "Returns the list of chained spans ordered from the first to the last.")
  (:method ((nul null))
    nil)
  (:method ((span span))
    (loop
      :for current = (first-span span) :then (next-span current)
      :collect current
      :until (last-span-p current))))

(defgeneric join-spans (left right)
  (:documentation "Concatenates the two doubly-linked lists referenced by the spans left and right.")
  (:method ((left span) (right span))
    (let ((fl (first-span left))
          (fr (first-span right)))
      (assert (not (eql fl fr)))
      (let ((ll (last-span left)))
        (setf (next (tail ll)) (head fr)
              (previous (head fr)) (tail ll))
        left))))


(defgeneric span-contents (span)
  (:method ((span span))
    (unless (emptyp span)
      (loop
        :for current = (head span) :then (next current)
        :unless (typep current 'place-holder-node) :collect current
        :until (tail-in-span-p current)))))

(defgeneric span-nth (index span)
  (:method (index (span span))
    (when (and (<= 0 index) (head span))
      (loop
        :for node = (head span) :then (next node)
        :do (assert node)
        :until (or (zerop index) (tail-in-span-p node))
        :unless (typep node 'place-holder-node) 
        :do (decf index)
        :finally (return (when (zerop index)
                           node))))))

(defgeneric span-length (span)
  (:method ((span span))
    (if (emptyp span)
        0
        (loop
          :for node = (head span) :then (next node)
          :do (assert node)
          :unless (typep node 'place-holder-node) 
          :count 1
          :until (tail-in-span-p node)))))



(defgeneric %update-node-span (span)
  (:method ((span span))
    (loop
      :for current = (head span) :then (next current)
      :do (setf (span current) span)
      :until (tail-in-span-p current))))


(defgeneric split-span-before (span node-or-index)
  (:method ((span span) (index integer))
    (split-span-before span (span-nth index span)))
  (:method ((span span) (node node))
    (assert (eql span (span node)))
    (if (head-in-span-p node)
        (let* ((new-span (make-empty-span))
               (place-holder (tail new-span)))
          (%insert-node-before place-holder (head span))
          (values new-span span))
        (let* ((new-head (head span))
               (new-tail (previous node))
               (new-span (make-instance 'span :head new-head :tail new-tail)))
          (%update-node-span new-span)
          (setf (head span) node)
          (values new-span span)))))

(defgeneric split-span-after (span node-or-index)
  (:method ((span span) (index integer))
    (split-span-after span (span-nth index span)))
  (:method ((span span) (node node))
    (assert (eql span (span node)))
    (if (tail-in-span-p node)
        (let* ((new-span (make-empty-span))
               (place-holder (tail new-span)))
          (%insert-node-after place-holder (tail span))
          (values span new-span))
        (let* ((new-head (next node))
               (new-tail (tail span))
               (new-span (make-instance 'span :head new-head :tail new-tail)))
          (%update-node-span new-span)
          (setf (tail span) node)
          (values span new-span)))))

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

(defgeneric forward-slurp-span (span)
  (:method ((span span))
    (let* ((tail (tail span))
           (head (next tail))
           (next-span (and head (span head))))
      (when (and next-span (not (emptyp next-span)))
        (if (emptyp span)
            (let ((next-tail (tail next-span)))
              (%insert-node-after (%delete-node tail) next-tail)
              (setf (head span) head
                    (tail span) next-tail
                    (head next-span) tail
                    (tail next-span) tail))
            (let ((empty-node (make-instance 'place-holder-node))
                  (next-tail (tail next-span)))
              (%insert-node-after empty-node next-tail) 
              (setf (tail span) next-tail
                    (head next-span) empty-node
                    (tail next-span) empty-node)))
        (%update-node-span span)
        (%update-node-span next-span))
      span)))

(defgeneric backward-slurp-span (span)
  (:method ((span span))
    (let* ((head (head span))
           (tail (previous head))
           (previous-span (and tail (span tail))))
      (when (and previous-span (not (emptyp previous-span)))
        (if (emptyp span)
            (let ((previous-head (head previous-span)))
              (%insert-node-before (%delete-node head) previous-head)
              (setf (head span) previous-head
                    (tail span) tail
                    (head previous-span) head
                    (tail previous-span) head))
            (let ((empty-node (make-instance 'place-holder-node))
                  (previous-head (head previous-span)))
              (%insert-node-before empty-node previous-head) 
              (setf (head span) previous-head
                    (head previous-span) empty-node
                    (tail previous-span) empty-node)))
        (%update-node-span span)
        (%update-node-span previous-span))
      span)))


(defgeneric span-prepend-node (span node)
  (:method ((span span) (node node))
    (setf (span node) span)
    (if (emptyp span)
        (setf (previous node) (previous (head span))
              (next node) (next (tail span))
              (head span) node
              (tail span) node)
        (setf (previous node) (previous (head span))
              (previous (head span)) node
              (next node) (head span)
              (head span) node))))

(defgeneric span-append-node (span node)
  (:method ((span span) (node node))
    (setf (span node) span)
    (if (emptyp span)
        (setf (previous node) (previous (head span))
              (next node) (next (tail span))
              (head span) node
              (tail span) node)
        (setf (next node) (next (tail span))
              (next (tail span)) node
              (previous node) (tail span)
              (tail span) node))))

(defgeneric remove-node (node)
  (:method ((node node))
    (let ((span (span node)))
      (when span
        (when (next node)
          (setf (previous (next node)) (previous node)))
        (when (previous node)
          (setf (next (previous node)) (next node)))
        (if (eql (head span) (tail span))
            (let ((place-holder (make-instance 'place-holder-node
                                               :previous (previous (head span))
                                               :next (next (tail span)))))
              (setf (head span) place-holder
                    (tail span) place-holder))
            (progn
              (when (eql node (head span))
                (setf (head span) (next node)))
              (when (eql node (tail span))
                (setf (tail span) (previous node))))))
      (setf (span node) nil)
      node)))

(defgeneric insert-node-after (node previous)
  (:method ((node node) (previous node))
    (setf (span node) (span previous))
    (%insert-node-after node previous)
    (when (eql previous (tail (span previous)))
      (setf (tail (span previous)) node))
    node))

(defgeneric insert-node-before (node next)
  (:method ((node node) (next node))
    (setf (span node) (span next))
    (%insert-node-before node next)
    (when (eql next (head (span next)))
      (setf (head (span next)) node))
    node))


(defgeneric find-node-if (predicate head)
  (:method (predicate (head node))
    (loop
      :for node = head :then (next node)
      :while node
      :when (and (not (typep node 'place-holder-node))
                 (funcall predicate node))
        :do (return-from find-node-if node))))


(defgeneric span-position-if (predicate span &key key start end)
  (:method (predicate (span span) &key (key (function identity)) (start 0) end)
    (when (head span)
      (loop
        :for index :from 0
        :for node = (head span) :then (next node)
        :do
           (assert node)
           (cond
             ((and (<= start index)
                   (or (null end) (< index end))
                   (funcall predicate (funcall key node)))
              (return-from span-position-if index))
             ((eql node (tail span))
              (return-from span-position-if nil)))))))


(defgeneric span-position (node span &key test key start end)
  (:method ((node node) (span span) &key (test (function eql)) (key (function identity)) (start 0) end)
    (let ((value (funcall key node)))
      (span-position-if (lambda (node) (funcall test value (funcall key node)))
                         span :key key :start start :end end))))





;; measure -> tempo
;;      t1            t2
;; -------------- -----------
;; No.  


;; measure -> line
;; m1 m2 m3 m4 m5 m6 m7 m8 m9
;; -------- -------- --------
;;   l1        l2       l3






;; (defclass partition (element)
;;   ((pages  :type owned-span)
;;    (tempos :type owned-span)))
;; 
;; (defclass line (graphic-elment numbered node span)
;;   ())
;; 
;; (defclass tempo (elment node span)
;;   ())
;; 
;; (defclass measure (graphic-elment numbered node span)
;;   ())




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


(defun test/spans ()
  (let ((empty (make-empty-span)))
    (assert (emptyp empty))
    (assert (zerop (span-length empty)))
    (assert (equal '() (span-contents empty))))
  (let* ((nodes (list (make-instance 'test/node)
                      (make-instance 'test/node)
                      (make-instance 'test/node)
                      (make-instance 'test/node)
                      (make-instance 'test/node)))
         (span (make-span nodes)))
    (assert (= (length nodes) (span-length span)))
    (assert (equal nodes (span-contents span)))
    (assert (headp (first nodes)))
    (assert (tailp (first (last nodes))))
    (assert (head-in-span-p (first nodes)))
    (assert (tail-in-span-p (first (last nodes))))
    (assert (every (lambda (node) (eql span (span node))) (span-contents span)))
    (assert (eql span (first-span span)))
    (assert (eql span (last-span span)))
    (assert (null (previous-span span)))
    (assert (null (next-span span)))
    (assert (first-span-p span))
    (assert (last-span-p span))
    (assert (equal (list span) (span-list span)))
    (loop
      :for node :in nodes
      :for index :from 0
      :do (assert (eql node (span-nth index span)))))
  :success)


(defun check-span-list (spans)
  (flet ((equiv (a b) (or (and a b) (and (not a) (not b)))))
    (let ((first (first spans))
          (last  (first (last spans))))
      (dolist (span spans)
        (assert (eql first (first-span span)))
        (assert (eql last  (last-span  span)))
        (assert (equal spans (span-list span))))
      (loop
        :for (previous current) :on (cons nil spans)
        :when current
          :do (assert (eql previous (previous-span current))))
      (loop
        :for (current next) :on (append spans (list nil))
        :when current
          :do (assert (eql next (next-span current))))
      (let ((first   (first spans))
            (last    (first (last spans)))
            (others  (butlast (rest spans))))
        (assert (first-span-p first))
        (dolist (span (cons first others))
          (assert (not (last-span-p span))))
        (assert (last-span-p last))
        (dolist (span (cons last others))
          (assert (not (first-span-p span)))))))
  :success)


(defun test/join-spans ()
  (flet ((check-join (s1 s2)
           (let* ((j  (join-spans s1 s2))
                  (n1 (span-contents s1))
                  (n2 (span-contents s2))
                  (c  (reduce (function append) (span-list j) :key (function span-contents))))
             (assert (eql s1 j))
             (assert (equal (list s1 s2) (span-list s1)))
             (assert (equal (list s1 s2) (span-list s2)))
             (assert (equal c (append n1 n2))))))
    (check-join (make-empty-span)
                (make-empty-span))
    (check-join (make-span (list (make-instance 'test/node)
                                 (make-instance 'test/node)
                                 (make-instance 'test/node)
                                 (make-instance 'test/node)
                                 (make-instance 'test/node)))
                (make-empty-span))
    (check-join (make-empty-span)
                (make-span (list (make-instance 'test/node)
                                 (make-instance 'test/node)
                                 (make-instance 'test/node)
                                 (make-instance 'test/node)
                                 (make-instance 'test/node))))
    (check-join (make-span (list (make-instance 'test/node)
                                 (make-instance 'test/node)
                                 (make-instance 'test/node)
                                 (make-instance 'test/node)
                                 (make-instance 'test/node)))
                (make-span (list (make-instance 'test/node)
                                 (make-instance 'test/node)
                                 (make-instance 'test/node)
                                 (make-instance 'test/node)
                                 (make-instance 'test/node)))))
  :success)


(defun test/split-spans ()
  (loop
    :for i :from 0 :to 4
    :do (let* ((nodes (list (make-instance 'test/node)
                            (make-instance 'test/node)
                            (make-instance 'test/node)
                            (make-instance 'test/node)
                            (make-instance 'test/node)))
               (span (make-span nodes)))
          (multiple-value-bind (a b) (split-span-before span i)
            (assert (= i (span-length a)))
            (assert (equalp nodes (append (span-contents a) (span-contents b))))
            (assert (every (lambda (node) (eql a (span node))) (span-contents a)))
            (assert (every (lambda (node) (eql b (span node))) (span-contents b)))
            (check-span-list (list a b))))
    :do (let* ((nodes (list (make-instance 'test/node)
                            (make-instance 'test/node)
                            (make-instance 'test/node)
                            (make-instance 'test/node)
                            (make-instance 'test/node)))
               (span (make-span nodes)))
          (multiple-value-bind (a b) (split-span-after span i)
            (assert (= (1+ i) (span-length a)))
            (assert (equalp nodes (append (span-contents a) (span-contents b))))
            (assert (every (lambda (node) (eql a (span node))) (span-contents a)))
            (assert (every (lambda (node) (eql b (span node))) (span-contents b))))))
  :success)


(defun test/span-append-node ()
  (let* ((nodes (list (make-instance 'test/node)
                      (make-instance 'test/node)
                      (make-instance 'test/node)
                      (make-instance 'test/node)
                      (make-instance 'test/node)))
         (span (make-empty-span)))
    (loop
      :for node :in nodes
      :do (span-append-node span node))
    (assert (equal nodes (span-contents span))))
  :success)


(defun test/span-prepend-node ()
  (let* ((nodes (list (make-instance 'test/node)
                      (make-instance 'test/node)
                      (make-instance 'test/node)
                      (make-instance 'test/node)
                      (make-instance 'test/node)))
         (span (make-empty-span)))
    (loop
      :for node :in nodes
      :do (span-prepend-node span node))
    (assert (equal (reverse nodes) (span-contents span))))
  :success)

(defun test/forward-slurp-span ()
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
      :do (let ((r1 (make-span nodes))
                r2 r3 r4)
            (multiple-value-setq (r1 r2) (split-span-before r1 2))
            (multiple-value-setq (r2 r3) (split-span-before r2 i))
            (multiple-value-setq (r3 r4) (split-span-before r3 (- (length nodes) 4 i)))
            #-(and)(progn (print (list (span-length r1)
                                       (span-length r2)
                                       (span-length r3)
                                       (span-length r4)))
                          (print (list (span-contents r1)
                                       (span-contents r2)
                                       (span-contents r3)
                                       (span-contents r4)))
                          (print '-->))
            (assert (equal nodes (append (span-contents r1)
                                         (span-contents r2)
                                         (span-contents r3)
                                         (span-contents r4))))
            (forward-slurp-span r2)
            #-(and) (progn (print (list (span-length r1)
                                        (span-length r2)
                                        (span-length r3)
                                        (span-length r4)))
                           (print (list (span-contents r1)
                                        (span-contents r2)
                                        (span-contents r3)
                                        (span-contents r4))))
            (assert (emptyp r3))
            (assert (= 2 (span-length r1)))
            (assert (= 2 (span-length r4)))
            (assert (equal nodes (append (span-contents r1)
                                         (span-contents r2)
                                         (span-contents r4))))
            (check-span-list (list r1 r2 r3 r4)))))
  :success)

(defun test/backward-slurp-span ()
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
      :do (let ((r1 (make-span nodes))
                r2 r3 r4)
            (multiple-value-setq (r1 r2) (split-span-before r1 2))
            (multiple-value-setq (r2 r3) (split-span-before r2 i))
            (multiple-value-setq (r3 r4) (split-span-before r3 (- (length nodes) 4 i)))
            #-(and) (progn (print (list (span-length r1)
                                        (span-length r2)
                                        (span-length r3)
                                        (span-length r4)))
                           (print (list (span-contents r1)
                                        (span-contents r2)
                                        (span-contents r3)
                                        (span-contents r4)))
                           (print '-->))
            (assert (equal nodes (append (span-contents r1)
                                         (span-contents r2)
                                         (span-contents r3)
                                         (span-contents r4))))
            (backward-slurp-span r3)
            #-(and) (progn (print (list (span-length r1)
                                        (span-length r2)
                                        (span-length r3)
                                        (span-length r4)))
                           (print (list (span-contents r1)
                                        (span-contents r2)
                                        (span-contents r3)
                                        (span-contents r4))))
            (assert (emptyp r2))
            (assert (= 2 (span-length r1)))
            (assert (= 2 (span-length r4)))
            (assert (equal nodes (append (span-contents r1)
                                         (span-contents r3)
                                         (span-contents r4))))
            (check-span-list (list r1 r2 r3 r4)))))
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

(defun check-span (span)
  (check-node-dll (head span))
  (check-node-dll (tail span))
  :success)

(defmethod dump ((span span))
  (print 'forward)
  (loop :for current = (head span) :then (next current)
        :do (print current)
        :while (next current))
  (print 'backward)
  (loop :for current = (tail span) :then (previous current)
        :do (print current)
        :while (previous current)))

(defun test ()
  (test/spans)
  (test/split-spans)
  (test/span-append-node)
  (test/span-prepend-node)
  (test/forward-slurp-span)
  (test/backward-slurp-span)
  (test/join-spans))

(test)



#-(and) (progn
          (let ((span (make-empty-span))
                (n1 (make-instance 'test/node))
                (n2 (make-instance 'test/node))
                (n3 (make-instance 'test/node)))
            (span-append-node span n1)
            (check-span span)
            (span-contents span)
            (span-append-node span n2)
            (check-span span)
            (span-contents span)
            (span-append-node span n3)
            (check-span span)
            (span-contents span))

          (let ((span (make-empty-span))
                (n1 (make-instance 'test/node))
                (n2 (make-instance 'test/node))
                (n3 (make-instance 'test/node)))
            (span-append-node span n1)
            (check-span span)
            (span-contents span)
            (span-append-node span n2)
            (check-span span)
            (span-contents span)
            (span-append-node span n3)
            (check-span span)
            (values (span-contents span)
                    (list n1 n2 n3)))

          (let ((span (make-empty-span))
                (n1 (make-instance 'test/node))
                (n2 (make-instance 'test/node))
                (n3 (make-instance 'test/node)))
            (span-prepend-node span n1)
            (check-span span)
            (span-contents span)
            (span-prepend-node span n2)
            (check-span span)
            (span-contents span)
            (span-prepend-node span n3)
            (check-span span)
            (values (span-contents span)
                    (list n1 n2 n3)))


          (progn
            (print (span-contents span))
            
            (print (span-contents span))
            (span-append-node span n3)
            (print (span-contents span))))



;;;; THE END ;;;;
