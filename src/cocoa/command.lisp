;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               command.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Global commands.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-12-25 <PJB> Created.
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
;;;;    

(in-package "ABNOTATION.COMMAND")
(objcl:set-objective-cl-syntax)


(defcommand keyboard-quit ()
  (throw :petite-gazongue nil))


;; (defun ask-user (title question &rest options &key &allow-other-keys)
;;   "
;; DO:      Runs a modal panel asking the user a QUESTION.
;; 
;; OPTIONS: from one to tree key string options. The strings are used as
;;          button labels, the keys are the result returned corresponding
;;          to the user choice.
;; "
;;   (ecase (#_NSRunAlertPanel
;;           (to-objc title)
;;           (to-objc "%@")
;;           (if (nth 1 options)
;;             (to-objc (nth 1 options))
;;             (error "Missing at least one option."))
;;           (if (nth 3 options) (to-objc (nth 3 options)) oclo:*null*)
;;           (if (nth 5 options) (to-objc (nth 5 options)) oclo:*null*)
;;           :id (to-objc question))
;;     ((#.#$NSAlertDefaultReturn)   (nth 0 options))
;;     ((#.#$NSAlertAlternateReturn) (nth 2 options))
;;     ((#.#$NSAlertOtherReturn)     (nth 4 options))))


(defun ask-user (title question &rest options &key &allow-other-keys)
  "
DO:      Runs a modal panel asking the user a QUESTION.

OPTIONS: from one to tree key string options. The strings are used as
         button labels, the keys are the result returned corresponding
         to the user choice.
"
  (ecase [[NSAlert alertWithMessageText:(to-objc title)
                   defaultButton:(if (nth 1 options)
                                   (to-objc (nth 1 options))
                                   (error "Missing at least one option."))
                   alternateButton:(if (nth 3 options) (to-objc (nth 3 options)) oclo:*null*)
                   otherButton:(if (nth 5 options) (to-objc (nth 5 options)) oclo:*null*)
                   informativeTextWithFormat:(to-objc "%@") (:id)(to-objc question)] runModal]
    ((#.#$NSAlertDefaultReturn)   (nth 0 options))
    ((#.#$NSAlertAlternateReturn) (nth 2 options))
    ((#.#$NSAlertOtherReturn)     (nth 4 options))))


;; (ask-user "Title 1" "Question?" :default "Ok <>" :alternate "Alternative //" :other "Cancel !!")
;; (ask-user "Title 1" "Question?" :default "Ok <>" :cancel "Cancel !!")



(defun select-file ()
  (let ((panel [NSOpenPanel openPanel]))
    [panel setCanChooseFiles:YES]
    [panel setCanChooseDirectories:NO]
    [panel setResolvesAliases:YES]
    [panel setAllowsMultipleSelection:NO]
    [panel setTitle:(to-objc "Find File")]
    [panel setMessage:(to-objc "Select a partition or midi file:")]
    [panel setPrompt:(to-objc "Open")]
    [panel setCanCreateDirectories:NO]
    [panel setShowsHiddenFiles:NO]
    [panel setExtensionHidden:NO]
    ;; [panel setAllowedFileTypes:[]]
    (if (= [panel runModal] #$NSFileHandlingPanelOKButton)
      (let ((url [panel URL]))
        (if [url isFileURL]
          (to-lisp [url path])
          (keyboard-quit)))
      (keyboard-quit))))


(global-set-key '((#\g :control))                'keyboard-quit)
(global-set-key '((#\x :control) (#\g :control)) 'keyboard-quit)






(defun load-midi-file (path)
  "load a MIDI file [menu, (drag-and-drop midi-file)]"
  #|->partition|#)

(defun write-midi-file (partition path)
  "export a MIDI file [menu]")

(defun load-partition-file (path)
  "load an abnotation partition file [menu, (drag-and-drop midi-file)]"
  #|->partition|#)

(defun write-partition-file (partition path)
  "save an abnotation partition file [menu]")

  

(defgeneric select (element)
  (:documentation "Select an element.
- sélections [graphique (simple clic, shift-click, command-click), & bindings]
  + annotation (texte ou image)
  + note
    * head
    * accidental
    * beam
    * dynamic
    * tenue
  + cluster
    idem note
  + measure
  + line
  + page
  + partition
"))


;; - sur l'objet sélectionné:
;;   + ajout d'une annotation (image ou texte) (tous sauf annotations elles mêmes) [menu].
;;   + suppression de l'annotation [menu].
;;   + ajustement de la position (line, ledger, staff, clef, sound, beam,
;;     dynamic, tenue, head, accidental, annotation) [click-and-drag, ou saisie offset (+/-9.99 mm)]
;;   + cut de l'objet [menu].
;;   + copy de l'objet [menu].
;;   + delete de l'objet [menu].
;;   + paste of an object -> insertion [menu].
;;   + paste of an image or text -> add/replace the annotation [menu].



;; - sur la partitions (paramètres globaux):
;;   + changement de la taille du papier et orientation. [menu]
;;   + changement de la hauteur des portées (3 mm, 5 mm, 7 mm). [menu]
;;   + changement du nombre de portées par ligne et clefs. [menu]
;;   + changement de la police des numéros de page. [menu]
;;   + changement de la police des numéros de ligne. [menu]
;;   + changement de la police des numéros de mesure. [menu]
;;   + édition des métadonnées (titre, auteur, annotation).


;; - sur measure(s) sélectionnée(s):
;;   + ajustement de la largeur (measure) [click-and-drag on corner, ou saisie durée (9.99 s)].
;;   + changement du tempo second/measure (ajustement de la largeur automatique).
;;   + ajustement de la position du numéro.
;; 
;; - édition de notes:
;; 
;;   + click-and-drag on the head to change the pitch and start of the
;;     note. (if multiple selection, transpose or offset all the
;;     selection). This may add or remove accidentals.
;; 
;;   + click-and-drag on the right part of the beam to change the duration of the note.
;;     (variation to shift the following notes or not). This may add or remove a tenue.
;; 
;;   + add a dynamic [popup-menu]
;; 
;;   + click-and-drag on the dynamic to change it.

;; - insertion d'une page
;; - insertion d'une ligne
;; - insertiotn d'une mesure
;; - insertion d'une note (en cluster ou en séquence)

;; - selection of an image annotation
;; - cut/copy/paste/delete of the image annotation
;; - selection of a text annotation
;; - edition of rich text annotation (font).


;; Other features
;; --------------------------------------------------------------------------------
;; 
;; - get a histogram of the dynamics
;; - specify the ranges for the various dynamics annotations: 𝆏𝆏𝆏 𝆏𝆏 𝆏 𝆐𝆑 𝆐𝆑 𝆑 𝆑𝆑 𝆑𝆑𝆑
;; - selection of digits or letters to annotate the dynamics.
;; - identification of ranges of monotone dynamics changes.   𝆒   𝆓
;; 
;; - representation of the partition as an editable, textual, lispy structure.


;;;; THE END ;;;;
