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
  )

(defun staves/bass ()
  ;; 𝄢
  )

(defun staves/treble+bass ()
  ;; 𝄞𝄢
  )

(defun staves/treble8+treble+bass ()
  ;; 𝄟𝄞𝄢
  )

(defun staves/treble+bass+bass8va ()
  ;; 𝄞𝄢𝄤
  )

(defun staves/treble8+treble+bass+bass8vb ()
  ;; 𝄟𝄞𝄢𝄤
  (list (make-fiveline-staff :name "treble 8va"
                             :clef (clef/treble8)
                             :keysig)
        (make-fiveline-staff :name "treble"
                             :clef (clef/treble)
                             :keysig)
        (make-fiveline-staff :name "bass"
                             :clef (clef/bass)
                             :keysig)
        (make-fiveline-staff :name "bass 8vb"
                             :clef (clef/bass8vb)
                             :keysig))  
  )

