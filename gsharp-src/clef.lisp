(in-package :gsharp)

;; http://en.wikipedia.org/wiki/Clef
;; http://en.wikipedia.org/wiki/List_of_musical_symbols
;; http://www.phys.unsw.edu.au/jw/notes.html


(defun clef/french-violin   () (make-clef :treble :lineno 1))
(defun clef/treble          () (make-clef :treble :lineno 2))
(defun clef/soprano         () (make-clef :c      :lineno 1))
(defun clef/mezzo-soprano   () (make-clef :c      :lineno 2))
(defun clef/alto            () (make-clef :c      :lineno 3))
(defun clef/tenor           () (make-clef :c      :lineno 4))

(defun clef/baritone-c      () (make-clef :c      :lineno 5))
(defun clef/baritone-bass   () (make-clef :bass   :lineno 3))
(defun clef/bass            () (make-clef :bass   :lineno 4))
(defun clef/subbass         () (make-clef :bass   :lineno 5))

(defun clef/treble15ma      () (make-clef :treble15ma :lineno 2))
(defun clef/treble8va       () (make-clef :treble8va  :lineno 2))
(defun clef/treble8vb       () (make-clef :treble8vb  :lineno 2))
(defun clef/treble15mb      () (make-clef :treble15mb :lineno 2))

(defun clef/bass15ma        () (make-clef :bass15ma   :lineno 2))
(defun clef/bass8va         () (make-clef :bass8va    :lineno 2))
(defun clef/bass8vb         () (make-clef :bass8vb    :lineno 2))
(defun clef/bass15mb        () (make-clef :bass15mb   :lineno 2))


;; - choix du nombre de portées par ligne, avec les clefs: (𝄞 𝄢 𝄞𝄢 𝄞𝄸𝄞𝄢 𝄞𝄢𝄢𝄹 𝄞𝄸𝄞𝄢𝄢𝄹)
;;   (sol fa sol+fa sol15ma+sol+fa sol+fa+fa15mb sol15ma+sol+fa+fa15mb)  ::



(defun staves/treble ()
  ;; "𝄞"
  (list (make-fiveline-staff :name "treble"
                             :clef (clef/treble))))

(defun staves/bass ()
  ;; "𝄢"
  (list (make-fiveline-staff :name "bass"
                             :clef (clef/bass))))

(defun staves/treble+bass ()
  ;; "𝄞 𝄢"
  (list (make-fiveline-staff :name "treble"
                             :clef (clef/treble))
        (make-fiveline-staff :name "bass"
                             :clef (clef/bass))))

(defun staves/treble15ma+treble+bass ()
  ;; "𝄞𝄸 𝄞 𝄢" 
  (list (make-fiveline-staff :name "treble 15ma"
                             :clef (clef/treble15ma))
        (make-fiveline-staff :name "treble"
                             :clef (clef/treble))
        (make-fiveline-staff :name "bass"
                             :clef (clef/bass))))

(defun staves/treble+bass+bass15mb ()
  ;;  "𝄞 𝄢 𝄢𝄹"
  (list (make-fiveline-staff :name "treble"
                             :clef (clef/treble))
        (make-fiveline-staff :name "bass"
                             :clef (clef/bass))
        (make-fiveline-staff :name "bass 15mb"
                             :clef (clef/bass15mb))))

(defun staves/treble15ma+treble+bass+bass15mb ()
  ;; "𝄞𝄸 𝄞 𝄢 𝄢𝄹"
  (list (make-fiveline-staff :name "treble 15ma"
                             :clef (clef/treble15ma))
        (make-fiveline-staff :name "treble"
                             :clef (clef/treble))
        (make-fiveline-staff :name "bass"
                             :clef (clef/bass))
        (make-fiveline-staff :name "bass 15mb"
                             :clef (clef/bass15mb))))

(defparameter *abnotation/staves* '(staves/trebble
                                    staves/bass
                                    staves/treble+bass
                                    staves/treble15ma+treble+bass
                                    staves/treble+bass+bass15mb
                                    staves/treble15ma+treble+bass+bass15mb))

;;;; THE END ;;;;
