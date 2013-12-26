;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               transformation.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    CLIM-like affine transformations API.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-12-26 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2013 - 2013
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

(define-condition transformation-error (error)
  ())
(define-condition transformation-underspecified (transformation-error)
  ((points :initarg :points :reader reflection-underspecified-points)))
(define-condition reflection-underspecified (transformation-underspecified)
  ())
(define-condition singular-transformation (transformation-error)
  ((transformation :initarg :transformation :reader singular-transformation-transformation)))


(defun transformationp (object)
  (and (vectorp object)
       (= 6 (length object))
       (every (function realp) object)))

(defparameter +identity-transformation+ (make-identity-matrix))

(defun make-translation-transformation (x y)
  (make-translation-matrix x y))

(defun make-rotation-transformation (angle &optional origin)
  (if origin
    (make-rotation-transformation* angle (point-x origin) (point-y origin))
    (make-rotation-matrix angle)))

(defun make-rotation-transformation* (angle &optional origin-x origin-y)
  (if (or origin-x origin-y)
    (matrix-multiply (make-translation-matrix origin-x origin-y)
                     (matrix-multiply (make-rotation-matrix angle)
                                      (make-translation-matrix (- origin-x) (- origin-y))))
    (make-rotation-matrix angle)))


(defun make-scaling-transformation (kx ky &optional origin)
  (if origin
    (make-scaling-transformation* kx ky (point-x origin) (point-y origin))
    (make-scale-matrix kx ky)))

(defun make-scaling-transformation* (kx ky &optional origin-x origin-y)
  (if (or origin-x origin-y)
    (matrix-multiply (make-translation-matrix origin-x origin-y)
                     (matrix-multiply (make-scale-matrix kx ky)
                                      (make-translation-matrix (- origin-x) (- origin-y))))
    (make-scale-matrix kx ky)))

(defun make-reflection-transformation (pt1 pt2)
  (make-reflection-transformation* (point-x pt1) (point-y pt1) (point-x pt2) (point-y pt2)))

(defun make-reflection-transformation* (x1 y1 x2 y2)
  (when (and (= x1 x2) (= y1 y2))
    (error 'reflection-underspecified :points (list (point x1 y1) (point x2 y2))))
  (let* ((a (point x1 y1))
         (b (point x2 y2))
         (v (vector- b a))
         (z (point 0 0))
         (u (transform-distance (make-rotation-matrix (/ pi 2)) (size-width v) (size-height v)))
         (i (line-intersection a v z (size (point-x u) (point-y u))))
         (o (vector-angle v (size 1 0))))
    (reduce (function matrix-multiply)
            (list
             (make-translation-matrix (- (point-x i)) (- (point-y i)))
             (make-rotation-matrix o)
             (make-matrix 1 0 0 0 -1 0)
             (make-rotation-matrix (- o))
             (make-translation-matrix (point-x i) (point-y i))))))

;; (let ((mirror  (make-reflection-transformation* -2 0 2 2)))
;;   (mapcar (lambda (pt) (print (apply (function transform-point) mirror pt)))
;;           '((-1 -2)
;;             (0 1)
;;             (1 -1)
;;             (2 -3)
;;             (3 0))))

(defun make-transformation (xx xy yx yy tx ty)
  (check-type xx real)
  (check-type xy real)
  (check-type yx real)
  (check-type yy real)
  (check-type tx real)
  (check-type ty real)
  (make-matrix xx xy tx yx yy ty))


;; (defun make-3-point-transformation (pt1 pt2 pt3 pt1i pt2i pt3i)
;;   (when (zerop (determinant (vector- pt1 pt2) (vector- pt1 pt3)))
;;     (error 'transformation-underspecified :points (list pt1 pt2 pt3)))
;;   )

(defun transform-region (transformation region)
  (let ((output (create-path)))
    (add-path output transformation region)
    output))

