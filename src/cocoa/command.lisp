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
      (progn
        [panel URL])
      (keyboard-quit))))


(defcommand find-file ()
  (message "Let's open the file ~S" (select-file)))



(global-set-key '((#\g :control))                'keyboard-quit)
(global-set-key '((#\x :control) (#\g :control)) 'keyboard-quit)
(global-set-key '((#\x :control) (#\f :control)) 'find-file)





(defcommand zoom-in ()
  (setf (abnotation.cocoa::zoom (current-view)) (* (abnotation.cocoa::zoom (current-view)) 2)))

(defcommand zoom-out ()
  (setf (abnotation.cocoa::zoom (current-view)) (/ (abnotation.cocoa::zoom (current-view)) 2)))


(let ((abview-km (create-keymap "ABView" (global-keymap))))
  (keymap-set-key abview-km '((#\x :control) #\+) 'zoom-in)
  (keymap-set-key abview-km '((#\x :control) #\=) 'zoom-in)
  (keymap-set-key abview-km '((#\x :control) #\-) 'zoom-out)
  (setf (current-keymap) abview-km))


;;;; THE END ;;;;
