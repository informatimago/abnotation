;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               abwindow.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    ABWINDOW: The main window of ABNotation
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-12-10 <PJB> Created.
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
(in-package "ABNOTATION.COCOA")
(objcl:set-objective-cl-syntax)

(defparameter *default-window-frame* (make-rect :x 10 :y 100 :width 1000 :height 700))
(defvar *windows* '())



@[NSWindow subClass:ABWindow
           slots: ((partition           :initform nil :accessor partition)
                   (partition-subview   :initform nil :accessor partition-subview)
                   (command-subview     :initform nil :accessor command-subview)
                   (output-subview      :initform nil :accessor output-subview)
                   (split-subview       :initform nil :accessor split-subview))]

@[ABWindow method:(becomeKeyWindow)
           resultType:(:void)
           body:
           [super becomeKeyWindow]
           (setf *minibuffer*     (command-subview self)
                 *message*        (output-subview  self)
                 *current-window* self
                 *current-view*   (partition-subview self))]

@[ABWindow method:(resignKeyWindow)
           resultType:(:void)
           body:
           [super resignKeyWindow]
           (setf *minibuffer*     nil
                 *message*        *trace-output*
                 *current-window* nil
                 *current-view*   nil)]

@[ABWindow method:(windowShouldClose: (:id)sender)
           resultType:(:<bool>)
           body:
           (declare (ignore sender))
           (cond
            ((null (partition self)) t)
            ((needs-saving (partition self))
             (case (ask-user "Close Window"
                             "Shall we save the partition?"
                             :cancel "Cancel"
                             :save "Save and Close"
                             :discard "Discard and Close")
               (:cancel  (message "Cancelled")
                         nil)
               (:save    (message "Saving Not implemented yet")
                         nil)
               (:discard (message "Discarding changes")
                         t)))
            (t t))]

@[ABWindow method:(close)
           resultType:(:void)
           body:
           (setf *windows* (remove self *windows*))
           [super close]]


@[ABWindow method:(keyDown:(:id #|NSEvent|#)event)
           resultType:(:void)
           body: (process-key-event event :window self)]


@[ABWindow method:(appendOutput:(:id #|NSString|#)message)
           resultType:(:void)
           body:[(output-subview self) appendOutput:message]]

(defmethod format ((window ab-window) control-string &rest arguments)
  (let ((message [(to-objc (cl:format nil "~?" control-string arguments)) retain]))
    (on-main-thread [window appendOutput:message] :wait nil)))





(defun create-abwindow (rect &key
                             (title "Untitled AB Partition")
                             (partition (create-partition *staves/bass15mb-trebble15ma*)))
  (let* ((title (if (stringp title)
                  (unwrap title)
                  title))
         (window  [[ABWindow alloc]
                   initWithContentRect:(unwrap rect)
                   styleMask:(logior  #$NSTitledWindowMask 
                                      #$NSClosableWindowMask   
                                      #$NSMiniaturizableWindowMask 
                                      #$NSResizableWindowMask)
                   backing:#$NSBackingStoreBuffered
                   defer:nil])
         (command-rect   (make-rect :x 0 :y 0
                                    :width (width rect) :height  32))
         (split-rect     (make-rect :x 0 :y (top command-rect)
                                    :width (width rect)
                                    :height (- (height rect)
                                               (top command-rect))))
         (output-rect    (make-rect :x 0 :y 0
                                    :width (width rect) :height 100))
         (partition-rect (make-rect :x 0 :y (height output-rect)
                                    :width (width rect)
                                    :height (- (height split-rect) (height output-rect))))
         (partition-subview [[ABView alloc] initWithFrame:(to-objc partition-rect)])
         (output-subview    [[ABTextView  alloc] initWithFrame:(to-objc output-rect)])
         (command-subview   [[ABTextField alloc] initWithFrame:(to-objc command-rect)])
         (split-subview     (split-view split-rect
                                        (list (height output-rect)
                                              (scroll-view output-rect output-subview)
                                              (scroll-view partition-rect partition-subview))
                                        :divider-style :thick)))
    [[[partition-subview enclosingScrollView] contentView] setCopiesOnScroll:NO]
    (setf (partition window)         partition
          (partition-subview window) partition-subview
          (output-subview window)    output-subview
          (command-subview window)   command-subview
          (split-subview window)     split-subview)
    [window setTitle:title]
    [[window contentView] addSubview:split-subview]
    [[window contentView] addSubview:command-subview]
    [window setInitialFirstResponder:partition-subview]
    (compute-frame-and-bounds partition-subview)
    (setf (partition partition-subview) (partition window))
    ;; (layout-partition-from-file (partition window))
    (format window "Hello World!~%")
    window))




;; (defun initialize-keymaps ()
;;   (let ((dmap (create-keymap "global")))
;;     
;;     (let ((kmap (create-keymap "ABTextView" dmap)))
;;       
;;       )
;;     (let ((kmap (create-keymap "ABTextField" dmap)))
;;       
;;       )
;;     (let ((kmap (create-keymap "ABView" dmap)))
;;       
;;       )))
;; 
;; (initialize-keymaps)



(defcommand find-file ()
    (let ((path (select-file)))
      (message "Find file ~S" path)
      (let ((partition (create-partition *staves/bass15mb-trebble15ma*)))
        (extract-node (head (head (head partition))))
        (append-midi-sequence partition  (abnotation-read-midi-file path))
        (let ((window (create-abwindow *default-window-frame*
                                       :title path
                                       :partition partition)))
          (push window *windows*)
          [window makeKeyAndOrderFront:window]
          (layout partition)))
      path))

(defcommand kill-buffer ()
  [*current-window* performClose:*current-window*])

(global-set-key '((#\x :control) (#\f :control)) 'find-file)
(global-set-key '((#\x :control) #\k) 'kill-buffer)

(global-set-key '((#\v :alternate)) 'scroll-up)
(global-set-key '((#\v :control))   'scroll-down)




;; (global-set-key '((#\f :control :command))   'forward-page)
;; (global-set-key '((#\b :control :command))   'backward-page)
;; (global-set-key '((#\o :control :command))   'insert-page)


;; (inspect (global-keymap))


