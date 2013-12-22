;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               matrix.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Transformation matrices.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-12-22 <PJB> Adapted for ABNotation.
;;;;    2011-05-12 <PJB> Created
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2011 - 2013
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

(in-package "ABNOTATION.CORE")


;;;---------------------------------------------------------------------
;;;
;;; TRANSFORMATION MATRICES
;;;
;;; We implement transformation matrix arithmetic in lisp.  These
;;; transformation matrix are used to set or get the current
;;; transformation matrix of the canvas (Accessor
;;; TRANSFORMATION-MATRIX), and to multiply the transformation matrix
;;; (TRANSFORM).  

(defun make-matrix (xx xy x0 yx yy y0)
  (vector xx xy x0 yx yy y0))

(defun make-identity-matrix ()
  (make-matrix 1.0 0.0 0.0 0.0 1.0 0.0))

(defun make-rotation-matrix (radians)
  (let ((c (cos radians))
        (s (sin radians)))
    (make-matrix c (- s) 0.0 s c 0.0)))

(defun make-scale-matrix (sx sy)
  (make-matrix sx 0.0 0.0 0.0 sy 0.0))

(defun make-translation-matrix (tx ty)
  (make-matrix 1.0 0.0 tx 0.0 1.0 ty))

(defun matrixp (object)
  (and (vectorp object)
       (= 6 (length object))
       (every (function realp) object)))


(defun matrix-invert (matrix)
  (let ((a0 (aref matrix 0))
        (a1 (aref matrix 1))
        (a2 (aref matrix 2))
        (a3 (aref matrix 3))
        (a4 (aref matrix 4))
        (a5 (aref matrix 5)))
    (flet ((det (a b c d)  (- (* a d) (* b c))))
      (declare (inline det))
      (let ((det (det a0 a1 a3 a4)))
        (if (zerop det)
          (error "Invalid afine transformation matrix (nul determinant).")
          (make-matrix (/ a4 det)     (- (/ a1 det)) (/ (det a1 a2 a4 a5) det)
                       (- (/ a3 det)) (/ a0 det)     (/ (det a3 a5 a0 a2) det)))))))


(defun matrix-multiply (a b)
  (let ((a0 (aref a 0))
        (a1 (aref a 1))
        (a2 (aref a 2))
        (a3 (aref a 3))
        (a4 (aref a 4))
        (a5 (aref a 5))
        (b0 (aref b 0))
        (b1 (aref b 1))
        (b2 (aref b 2))
        (b3 (aref b 3))
        (b4 (aref b 4))
        (b5 (aref b 5)))
   (make-matrix (+ (* a0 b0) (* a3 b1))
                (+ (* a1 b0) (* a4 b1))
                (+ (* a2 b0) (* a5 b1) b2)
                (+ (* a0 b3) (* a3 b4))
                (+ (* a1 b3) (* a4 b4))
                (+ (* a2 b3) (* a5 b4) b5))))


;; (eval-when (:execute :compile-toplevel)
;; 
;;   (defun symbolic-make-rotation-matrix (radians)
;;     (let ((c `(cos ,radians))
;;           (s `(sin ,radians)))
;;       (make-matrix c `(- ,s) 0 s c 0)))
;;   
;;   (defun simplify (simple-expression)
;;     (if (atom simple-expression)
;;       simple-expression
;;       (case (first simple-expression)
;;         ((+) (let ((args (remove 0 (mapcar (function simplify) (rest simple-expression)))))
;;                (case (length args)
;;                  ((0)       0)
;;                  ((1)       (first args))
;;                  (otherwise `(+ ,@args)))))
;;         ((*) (let ((args (remove 1 (mapcar (function simplify) (rest simple-expression)))))
;;                (if (find 0 args)
;;                  0
;;                  (case (length args)
;;                    ((0)       1)
;;                    ((1)       (first args))
;;                    (otherwise `(* ,@args))))))
;;         (otherwise simple-expression))))
;; 
;;   (defun symbolic-matrix-multiply (a b)
;;     (let ((a0 (aref a 0))
;;           (a1 (aref a 1))
;;           (a2 (aref a 2))
;;           (a3 (aref a 3))
;;           (a4 (aref a 4))
;;           (a5 (aref a 5))
;;           (b0 (aref b 0))
;;           (b1 (aref b 1))
;;           (b2 (aref b 2))
;;           (b3 (aref b 3))
;;           (b4 (aref b 4))
;;           (b5 (aref b 5)))
;;       (map 'vector (function simplify)
;;            (make-matrix `(+ (* ,a0 ,b0) (* ,a3 ,b1))
;;                         `(+ (* ,a1 ,b0) (* ,a4 ,b1))
;;                         `(+ (* ,a2 ,b0) (* ,a5 ,b1) ,b2)
;;                         `(+ (* ,a0 ,b3) (* ,a3 ,b4))
;;                         `(+ (* ,a1 ,b3) (* ,a4 ,b4))
;;                         `(+ (* ,a2 ,b3) (* ,a5 ,b4) ,b5))))))
;; 
;; (defmacro generate-with-matrix (expression)
;;   `(let ((a0 (aref matrix 0))
;;          (a1 (aref matrix 1))
;;          (a2 (aref matrix 2))
;;          (a3 (aref matrix 3))
;;          (a4 (aref matrix 4))
;;          (a5 (aref matrix 5)))
;;      (make-matrix ,@(coerce (eval expression) 'list))))


(defun matrix-scale     (matrix sx sy)
  ;; (macroexpand '(generate-with-matrix
  ;;   (symbolic-matrix-multiply (make-matrix 'a0 'a1 'a2 'a3 'a4 'a5)
  ;;                             (make-scale-matrix 'sx 'sy))))
  (LET ((A0 (AREF MATRIX 0))
        (A1 (AREF MATRIX 1))
        (A2 (AREF MATRIX 2))
        (A3 (AREF MATRIX 3))
        (A4 (AREF MATRIX 4))
        (A5 (AREF MATRIX 5)))
    (MAKE-MATRIX (+ (* A0 SX) (* A3 0.0))
                 (+ (* A1 SX) (* A4 0.0))
                 (+ (* A2 SX) (* A5 0.0) 0.0)
                 (+ (* A0 0.0) (* A3 SY))
                 (+ (* A1 0.0) (* A4 SY))
                 (+ (* A2 0.0) (* A5 SY) 0.0))))


(defun matrix-translate (matrix tx ty)
  ;; (macroexpand '(generate-with-matrix
  ;;   (symbolic-matrix-multiply (make-matrix 'a0 'a1 'a2 'a3 'a4 'a5)
  ;;                             (make-translation-matrix 'tx 'ty))))
  (LET ((A0 (AREF MATRIX 0))
        (A1 (AREF MATRIX 1))
        (A2 (AREF MATRIX 2))
        (A3 (AREF MATRIX 3))
        (A4 (AREF MATRIX 4))
        (A5 (AREF MATRIX 5)))
    (MAKE-MATRIX (+ (* A0 1.0) (* A3 0.0))
                 (+ (* A1 1.0) (* A4 0.0))
                 (+ (* A2 1.0) (* A5 0.0) TX)
                 (+ (* A0 0.0) (* A3 1.0))
                 (+ (* A1 0.0) (* A4 1.0))
                 (+ (* A2 0.0) (* A5 1.0) TY))))


(defun matrix-rotate    (matrix radians)
  (let ((a0 (aref matrix 0))
        (a1 (aref matrix 1))
        (a2 (aref matrix 2))
        (a3 (aref matrix 3))
        (a4 (aref matrix 4))
        (a5 (aref matrix 5))
        (c (cos radians))
        (s (sin radians)))
    (make-matrix (- (* A0 c) (* A3 s))
                 (- (* A1 c) (* A4 s))
                 (- (* A2 c) (* A5 s))
                 (+ (* A0 s) (* A3 c))
                 (+ (* A1 s) (* A4 c))
                 (+ (* A2 s) (* A5 c)))))




(defun matrix-xx (matrix) (aref matrix 0))
(defun matrix-xy (matrix) (aref matrix 1)) 
(defun matrix-x0 (matrix) (aref matrix 2)) 
(defun matrix-yx (matrix) (aref matrix 3)) 
(defun matrix-yy (matrix) (aref matrix 4)) 
(defun matrix-y0 (matrix) (aref matrix 5)) 
(declaim (inline matrix-xx matrix-xy matrix-x0 matrix-yx matrix-yy matrix-y0))



(defun transform-point (matrix x y)
  (let ((a0 (aref matrix 0))
        (a1 (aref matrix 1))
        (a2 (aref matrix 2))
        (a3 (aref matrix 3))
        (a4 (aref matrix 4))
        (a5 (aref matrix 5)))
   (point (+ (* x a0) (* y a1) a2)
          (+ (* x a3) (* y a4) a5))))


(defun transform-distance (matrix dx dy)
  (let ((a0 (aref matrix 0))
        (a1 (aref matrix 1))
        (a2 (aref matrix 2))
        (a3 (aref matrix 3))
        (a4 (aref matrix 4))
        (a5 (aref matrix 5)))
   (point (+ (* dx a0) (* dy a1))
          (+ (* dx a3) (* dy a4)))))





(defconstant +epsilon+ 1e-12)

(defun matrix-equal (a b)
  (and (matrixp a)
       (matrixp b)
       (every (lambda (x y)
                  (or (= x y)
                   (and (zerop x) (< (abs y) +epsilon+))
                   (and (not (zerop x))
                        (< (abs (/ (- x y) x)) +epsilon+))))
              a b)))


(defun test/matrix ()
  (every (lambda (m)
             (let* ((m* (matrix-invert m))
                    (i  (matrix-multiply m m*)))
               (assert (matrix-equal (make-identity-matrix) i)
                       ()
                       "Matrix-Invert didn't work: ~A * ~A = ~A" m m* i)
               t))
         (list (make-rotation-matrix (/ pi 3))
               (make-translation-matrix 10.0 20.0)
               (make-scale-matrix 2.2 3.3)
               (matrix-multiply (matrix-multiply (make-rotation-matrix (/ pi 3))
                                                 (make-translation-matrix 10.0 20.0))
                                (make-scale-matrix 2.2 3.3))
               (matrix-multiply (matrix-multiply (make-scale-matrix 2.2 3.3)
                                                 (make-translation-matrix 10.0 20.0))
                                (make-rotation-matrix (/ pi 3)))))
  :success)

(declaim (inline make-matrix make-identity-matrix
                 make-rotation-matrix make-scale-matrix make-translation-matrix))


;;;; THE END ;;;;
