;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               file.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Serialize and deserialize a partition.
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


#|

(pprint (let ((partition (create-partition *staves/bass15mb-trebble15ma*)))
   (setf (tempo-and-notes partition) (abnotation-read-midi-file  #P"~/works/abnotation/abnotation/files/1-canal.mid"))
   partition))
|#

;; (defun test/file ()
;;   (let ((partition (create-partition *staves/bass15mb-trebble15ma*)))
;; 
;;     (setf (tempo-and-notes partition) (abnotation-read-midi-file  #P"~/works/abnotation/abnotation/files/1-canal.mid"))
;;     (layout-partition-from-tempos partition)))


(defparameter *partition* nil)
;; (setf *partition*  (test/file))
;; (mapcar (lambda (b) (list (minimum-lane b)
;;                           (maximum-lane b)
;;                           b))
;;         (bands (first (lines (first (pages *partition*))))))


(defun band-for-note (bands note)
  (let ((lane (lane (pitch note))))
    (loop
     :for band :in bands
     :when (<= (minimum-lane band) lane (maximum-lane band))
     :do (return-from band-for-note band))
    (error "No band for note ~S" note)))


;; (let* ((note (first (sounds (first (measures (first (tempos *partition*)))))))
;;       (band (band-for-note (bands (first (lines (first (pages *partition*))))) note)))
;;   (values band
;;           (- (lane (pitch note)) (minimum-lane band))
;;           (- (lane (pitch note)) (bottom-lane band))))

;; (staff :clef (clef :name :treble :line 2 :pitch 79 "#x30200394422D") "#x302003943A6D")
;; 1
;; 0
;; 
;; (pprint (test/file))



;;;; THE END ;;;;

