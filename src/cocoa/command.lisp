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





;;;; THE END ;;;;
