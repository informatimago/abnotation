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


(defun clef/treble8va       () (make-clef :treble8va :lineno 2))
(defun clef/bass8vb         () (make-clef :bass8vb   :lineno 2))

;; - choix du nombre de portées par ligne, avec les clefs: (𝄞 𝄢 𝄞𝄢 𝄟𝄞𝄢 𝄞𝄢𝄤 𝄟𝄞𝄢𝄤)
;;   (sol fa sol+fa sol8+sol+fa sol+fa+fa8va sol8+sol+fa+fa8va) 



(defun staves/treble ()
  ;; 𝄞
  (list (make-fiveline-staff :name "treble"
                             :clef (clef/treble))))

(defun staves/bass ()
  ;; 𝄢
  (list (make-fiveline-staff :name "bass"
                             :clef (clef/bass))))

(defun staves/treble+bass ()
  ;; 𝄞𝄢
  (list (make-fiveline-staff :name "treble"
                             :clef (clef/treble))
        (make-fiveline-staff :name "bass"
                             :clef (clef/bass))))

(defun staves/treble8va+treble+bass ()
  ;; 𝄟𝄞𝄢
  (list (make-fiveline-staff :name "treble 8va"
                             :clef (clef/treble8va))
        (make-fiveline-staff :name "treble"
                             :clef (clef/treble))
        (make-fiveline-staff :name "bass"
                             :clef (clef/bass))))

(defun staves/treble+bass+bass8vb ()
  ;; 𝄞𝄢𝄤
  (list (make-fiveline-staff :name "treble"
                             :clef (clef/treble))
        (make-fiveline-staff :name "bass"
                             :clef (clef/bass))
        (make-fiveline-staff :name "bass 8vb"
                             :clef (clef/bass8vb))))

(defun staves/treble8va+treble+bass+bass8vb ()
  ;; 𝄟𝄞𝄢𝄤
  (list (make-fiveline-staff :name "treble 8va"
                             :clef (clef/treble8va))
        (make-fiveline-staff :name "treble"
                             :clef (clef/treble))
        (make-fiveline-staff :name "bass"
                             :clef (clef/bass))
        (make-fiveline-staff :name "bass 8vb"
                             :clef (clef/bass8vb))))

(defvar *abnotation/staves* '(staves/trebble
                              staves/bass
                              staves/treble+bass
                              staves/treble8va+treble+bass
                              staves/treble+bass+bass8vb
                              staves/treble8va+treble+bass+bass8vb))
