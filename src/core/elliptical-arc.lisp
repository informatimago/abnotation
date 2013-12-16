;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               elliptical-arc.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This file implements L. Maisonobe's algorithm to convert
;;;;    ellitical arcs to sets of Bézier's curves.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-12-16 <PJB> Adapted to abnotation.
;;;;    2011-05-14 <PJB> Created.
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

(defun vector4 (data)
  (make-array 4
              :element-type 'double-float
              :initial-contents data))
(declaim (inline vector4))

(defun coefficients (data)
  (make-array '(2 4)
              :element-type '(vector double-float 4)
              :initial-contents (mapcar (lambda (row)
                                            (mapcar (function vector4) row))
                                        data)))
(declaim (inline coefficients))


(defparameter *coeffs2low*
  (coefficients
   '(((  3.92478d0   -13.5822d0     -0.233377d0    0.0128206d0   )
      ( -1.08814d0     0.859987d0    0.000362265d0 0.000229036d0 )
      ( -0.942512d0    0.390456d0    0.0080909d0   0.00723895d0  )
      ( -0.736228d0    0.20998d0     0.0129867d0   0.0103456d0   ))
     (( -0.395018d0    6.82464d0     0.0995293d0   0.0122198d0   )
      ( -0.545608d0    0.0774863d0   0.0267327d0   0.0132482d0   )
      (  0.0534754d0  -0.0884167d0   0.012595d0    0.0343396d0   )
      (  0.209052d0   -0.0599987d0  -0.00723897d0  0.00789976d0  ))))
  "coefficients for error estimation
while using quadratic Bézier curves for approximation
0 < b/a < 1/4")

(defparameter *coeffs2high*
  (coefficients
   '(((  0.0863805d0 -11.5595d0     -2.68765d0     0.181224d0    )
      (  0.242856d0   -1.81073d0     1.56876d0     1.68544d0     )
      (  0.233337d0   -0.455621d0    0.222856d0    0.403469d0    )
      (  0.0612978d0  -0.104879d0    0.0446799d0   0.00867312d0  ))
     ((  0.028973d0    6.68407d0     0.171472d0    0.0211706d0   )
      (  0.0307674d0  -0.0517815d0   0.0216803d0  -0.0749348d0   )
      ( -0.0471179d0   0.1288d0     -0.0781702d0   2.0d0         )
      ( -0.0309683d0   0.0531557d0  -0.0227191d0   0.0434511d0   ))))  
  "coefficients for error estimation
while using quadratic Bézier curves for approximation
1/4 <= b/a <= 1")


(defparameter *safety2*
  (vector4 '(0.02d0  2.83d0  0.125d0  0.01d0))
  "safety factor to convert the \"best\" error approximation
into a \"max bound\" error")


(defparameter *coeffs3low*
  (coefficients
   '(((  3.85268d0    -21.229d0       -0.330434d0     0.0127842d0  ) 
      ( -1.61486d0      0.706564d0     0.225945d0     0.263682d0   ) 
      ( -0.910164d0     0.388383d0     0.00551445d0   0.00671814d0 ) 
      ( -0.630184d0     0.192402d0     0.0098871d0    0.0102527d0  ))
     (( -0.162211d0     9.94329d0      0.13723d0      0.0124084d0  ) 
      ( -0.253135d0     0.00187735d0   0.0230286d0    0.01264d0    ) 
      ( -0.0695069d0   -0.0437594d0    0.0120636d0    0.0163087d0  ) 
      ( -0.0328856d0   -0.00926032d0  -0.00173573d0   0.00527385d0 ))))
  "coefficients for error estimation
while using cubic Bézier curves for approximation
0 < b/a < 1/4")

(defparameter *coeffs3high*
  (coefficients
   '(((  0.0899116d0  -19.2349d0      -4.11711d0      0.183362d0   ) 
      (  0.138148d0    -1.45804d0      1.32044d0      1.38474d0    ) 
      (  0.230903d0    -0.450262d0     0.219963d0     0.414038d0   ) 
      (  0.0590565d0   -0.101062d0     0.0430592d0    0.0204699d0  ))
     ((  0.0164649d0    9.89394d0      0.0919496d0    0.00760802d0 ) 
      (  0.0191603d0   -0.0322058d0    0.0134667d0   -0.0825018d0  ) 
      (  0.0156192d0   -0.017535d0     0.00326508d0  -0.228157d0   ) 
      ( -0.0236752d0    0.0405821d0   -0.0173086d0    0.176187d0   ))))
  "coefficients for error estimation
while using cubic Bézier curves for approximation
1/4 <= b/a <= 1")

(defparameter *safety3*
  (vector4 '(0.001d0  4.98d0  0.207d0  0.0067d0 ))
  "safety factor to convert the \"best\" error approximation
into a \"max bound\" error")



(defun rational-function (x c)
  "Compute the value of a rational function.
This method handles rational functions where the numerator is
quadratic and the denominator is linear
X: absissa for which the value should be computed
C: coefficients array of the rational function
"
  (declare (type double-float x)
           (type (vector double-float 4) c))
  (the double-float
    (/ (+ (* x (+ (* x (aref c 0)) (aref c 1))) (aref c 2))
       (+ x (aref c 3)))))



(defun elliptical-arc (cx cy a b &optional (theta 0.0d0) (lambda1 0.0d0) (lambda2 2pi)
                                   (degree 2) (threshold 1.0d-10))
  "
Build an approximation of an elliptical arc from its canonical
geometrical elements.
CX:             abscissa of the center of the ellipse
CY:             ordinate of the center of the ellipse
A:              semi-major axis
B:              semi-minor axis
THETA:          orientation of the major axis with respect to the x axis
LAMBDA1:        start angle of the arc
LAMBDA2:        end angle of the arc
DEGREE:         
THRESHOLD:      Default flatness for Bézier curve approximation (greater than 1.0e-10).
RETURN:         a path
"
  (assert (find degree #(1 2 3)))
  (assert (<= 1.0d-10 threshold))
  (let* ((cx        (coerce cx        'double-float))
         (cy        (coerce cy        'double-float))
         (a         (coerce a         'double-float))
         (b         (coerce b         'double-float))
         (theta     (coerce theta     'double-float))
         (cosTheta  (cos theta))
         (sinTheta  (sin theta))
         (lambda1   (coerce lambda1   'double-float))
         (lambda2   (coerce lambda2   'double-float))
         (threshold (coerce threshold 'double-float)))
    (declare (type double-float cx cy a b theta cosTheta sinTheta lambda1 lambda2 threshold)
             (type (integer 1 3) degree))    
   (flet ((estimate-error (degree etaA etaB)
            "Estimate the approximation error for a sub-arc of the instance.
DEGREE:  degree of the Bézier curve to use (1  2 or 3)
ETAA:    angle of the sub-arc
ETAB:    end angle of the sub-arc
RETURN:  The upper bound of the approximation error between the Bézier
         curve and the real ellipse.
"
            (declare (type double-float etaA etaB)
                     (type (integer 1 3) degree))
            (let ((eta (/ (+ etaA etaB) 2)))
              (declare (type double-float eta))
              (if (< degree 2)
                (let*  ((aCosEtaA   (* a (cos etaA))) ; start point
                        (bSinEtaA   (* b (sin etaA)))
                        (xA         (+ cx (* aCosEtaA cosTheta) (* -1 bSinEtaA sinTheta)))
                        (yA         (+ cy (* aCosEtaA sinTheta) (* bSinEtaA cosTheta)))
                        (aCosEtaB   (* a (cos etaB))) ; end point
                        (bSinEtaB   (* b (sin etaB)))
                        (xB         (+ cx (* aCosEtaB cosTheta) (* -1 bSinEtaB sinTheta)))
                        (yB         (+ cy (* aCosEtaB sinTheta) (* bSinEtaB cosTheta)))
                        (aCosEta    (* a (cos eta))) ; maximal error point
                        (bSinEta    (* b (sin eta)))
                        (x          (+ cx (* aCosEta cosTheta) (* -1 bSinEta sinTheta)))
                        (y          (+ cy (* aCosEta sinTheta) (* bSinEta cosTheta)))
                        (dx  (- xA xB))
                        (dy  (- yB yA)))
                  (declare (type double-float aCosEtaA bSinEtaA xA yA aCosEtaB bSinEtaB xB yB aCosEta bSinEta x y dx dy))
                  (the double-float
                    (abs (/ (+ (- (* x dy) (* y dx))
                               (- (* xB yA) (* xA yB)))
                            (sqrt (+ (* dx dx) (* dy dy)))))))
                (let* ((x    (/ b a))
                       (dEta (- etaB etaA))
                       (cos2 (cos (* 2 eta)))
                       (cos4 (cos (* 4 eta)))
                       (cos6 (cos (* 6 eta)))
                       (coeffs)
                       (safety))
                  (declare (type double-float x dEta cos2 cos4 cos6))
                  ;; select the right coeficients set according to degree and b/a
                  (if (= degree 2)
                    (setf coeffs  (if (< x 0.25) *coeffs2Low* *coeffs2High*)
                          safety   *safety2*)
                    (setf coeffs  (if (< x 0.25) *coeffs3Low* *coeffs3High*)
                          safety   *safety3*))
                  (let ((c0 (+         (rational-function x (aref coeffs 0 0))
                                       (* cos2 (rational-function x (aref coeffs 0 1)))
                                       (* cos4 (rational-function x (aref coeffs 0 2)))
                                       (* cos6 (rational-function x (aref coeffs 0 3)))))
                        (c1 (+         (rational-function x (aref coeffs 1 0))
                                       (* cos2 (rational-function x (aref coeffs 1 1)))
                                       (* cos4 (rational-function x (aref coeffs 1 2)))
                                       (* cos6 (rational-function x (aref coeffs 1 3))))))
                    (declare (type double-float c0 c1))
                    (the double-float
                      (* (rational-function x  safety) a (exp (+ c0 (* c1 dEta)))))))))))
     (let ((eta1       (atan (/ (sin lambda1) b)
                             (/ (cos lambda1) a)))
           (eta2       (atan (/ (sin lambda2) b)
                             (/ (cos lambda2) a)))
           (etaA   0.0d0)
           (etaB   0.0d0))
       (declare (type double-float eta1 eta2  etaA etaB))

       ;; make sure we have eta1 <= eta2 <= eta1 + 2 PI
       (decf eta2 (* 2pi (floor (/ (- eta2 eta1) 2pi)))) 

       ;; the preceding correction fails if we have exactly et2 - eta1 = 2 PI
       ;; it reduces the interval to zero length
       (when (and (< pi (- lambda2 lambda1))
                  (< (- eta2 eta1) PI))
         (incf eta2 2pi))

       ;;  Compute the bounding box. 
       (let ((bOnA  (/ b a))
             etaXMin  etaXMax  etaYMin  etaYMax)
         (declare (type double-float bOnA etaXMin etaXMax etaYMin etaYMax))
         (if (< (abs sinTheta) 0.1)
           (let  ((tanTheta (/ sinTheta cosTheta)))
             (declare (type double-float tanTheta))
             (if (< cosTheta 0.0)
               (setf etaXMin (- (atan (* tanTheta bOnA)))
                     etaXMax (+ etaXMin PI)
                     etaYMin (- PI/2 (atan (/ tanTheta bOnA)))
                     etaYMax (+ etaYMin PI))
               (setf etaXMax (- (atan (* tanTheta bOnA)))
                     etaXMin (- etaXMax PI)
                     etaYMax (- pi/2 (atan (/ tanTheta bOnA)))
                     etaYMin (- etaYMax PI))))
           (let ((invTanTheta (/ cosTheta sinTheta)))
             (declare (type double-float invTanTheta))
             (if (< sinTheta 0)
               (setf etaXMax (+ pi/2 (atan (/ invTanTheta bOnA)))
                     etaXMin (- etaXMax PI)
                     etaYMin (atan (* invTanTheta bOnA))
                     etaYMax (+ etaYMin PI))
               (setf etaXMin (+ pi/2 (atan (/ invTanTheta bOnA)))
                     etaXMax (+ etaXMin PI)
                     etaYMax (atan (* invTanTheta bOnA))
                     etaYMin (- etaYMax PI)))))

         (decf etaXMin (* 2pi (floor (/ (- etaXMin eta1) 2pi))))
         (decf etaYMin (* 2pi (floor (/ (- etaYMin eta1) 2pi))))
         (decf etaXMax (* 2pi (floor (/ (- etaXMax eta1) 2pi)))) 
         (decf etaYMax (* 2pi (floor (/ (- etaYMax eta1) 2pi))))
        
         (let ((n 1))
           (declare (type fixnum n))           
           (loop           ; find the number of Bézier curves needed
                :with found = nil
                :while (and (not found) (< n 1024))
                :do (let ((dEta  (/ (- eta2 eta1) n)))
                      (declare (type double-float dEta))
                      (if (<= dEta pi/2)
                        (progn
                          (setf etaB eta1
                                found t)
                          (loop
                             :for i fixnum :from 0 :below n
                             :until found
                             :do (progn
                                   (setf etaA etaB)
                                   (incf etaB dEta)
                                   (setf found (<= (estimate-error degree etaA etaB) threshold)))))
                        (setf n (* 2 n)))))
           (setf etaB  eta1)
           (let* ((path      (create-path))
                  (dEta      (/ (- eta2 eta1) n))
                  (cosEtaB   (cos etaB))
                  (sinEtaB   (sin etaB))
                  (aCosEtaB  (* a cosEtaB))
                  (bSinEtaB  (* b sinEtaB))
                  (aSinEtaB  (* a sinEtaB))
                  (bCosEtaB  (* b cosEtaB))
                  (xB        (+ cx (* aCosEtaB cosTheta) (* -1 bSinEtaB sinTheta)))
                  (yB        (+ cy (* aCosEtaB sinTheta) (* bSinEtaB cosTheta)))
                  (xBDot     (- (+ (* bCosEtaB sinTheta) (* aSinEtaB cosTheta))))
                  (yBDot     (- (* bCosEtaB cosTheta) (* aSinEtaB sinTheta))))
             (declare (type double-float dEta etaB cosEtaB sinEtaB aCosEtaB bSinEtaB aSinEtaB bCosEtaB xB yB xBDot yBDot))
             ;; (if pie-slice-p
             ;;   (progn
             ;;     (move-to-coordinates path cx cy)
             ;;     (line-to-coordinates path xB yB))
             ;;   (move-to-coordinates path xB yB))

             (move-to-coordinates path xB yB)
             (let* ((tt    (tan (/ dEta 2)))
                    (alpha (* (sin dEta) (/ (1- (sqrt (+ 4 (* 3 tt tt))))3))))
               (declare (type double-float tt alpha))
               (loop
                  :for i fixnum :from 0 :below n
                  :do (let ((xA    xB)
                            (yA    yB)
                            (xADot xBDot)
                            (yADot yBDot))
                        (declare (type double-float xA yA xADot yADot))
                        (setf etaA etaB)
                        (incf etaB dEta)
                        (setf cosEtaB  (cos etaB)
                              sinEtaB  (sin etaB)
                              aCosEtaB (* a cosEtaB)
                              bSinEtaB (* b sinEtaB)
                              aSinEtaB (* a sinEtaB)
                              bCosEtaB (* b cosEtaB)
                              xB       (+ cx (* aCosEtaB cosTheta) (* -1 bSinEtaB sinTheta))
                              yB       (+ cy (* aCosEtaB sinTheta) (* bSinEtaB cosTheta))
                              xBDot    (- (+ (* aSinEtaB cosTheta) (* bCosEtaB sinTheta)))
                              yBDot    (- (* bCosEtaB cosTheta) (* aSinEtaB sinTheta)))
                        (case degree
                          ((1) (line-to-coordinates path xB yB))
                          ((2) (let  ((k (/ (- (* yBDot (- xB xA)) (* xBDot (- yB yA)))
                                            (- (* xADot yBDot)     (* yADot xBDot)))))
                                 (quad-curve-to-coordinates path 
                                                            (+ xA (* k xADot))  (+ yA (* k yADot)) 
                                                            xB yB)))
                          ((3) (curve-to-coordinates path 
                                                     (+ xA (* alpha xADot)) (+ yA (* alpha yADot)) 
                                                     (- xB (* alpha xBDot)) (- yB (* alpha yBDot)) 
                                                     xB yB))))))
             ;; (when pie-slice-p
             ;;   (close-path path))
             path)))))))


;;;; THE END ;;;;
