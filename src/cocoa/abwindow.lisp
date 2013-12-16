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

(defgeneric format (output control-string &rest arguments )
  (:method ((output null) control-string &rest arguments)
    (apply (function cl:format) output control-string arguments))
  (:method ((output (eql t)) control-string &rest arguments)
    (apply (function cl:format) output control-string arguments))
  (:method ((output stream) control-string &rest arguments)
    (apply (function cl:format) output control-string arguments)))


@[NSWindow subClass:ABWindow
           slots: ((partition        :initform (create-partition *staves/bass15mb-trebble15ma*)
                                     :accessor partition)
                   (partition-view   :initform nil :accessor partition-view)
                   (command-view     :initform nil :accessor command-view)
                   (output-view      :initform nil :accessor output-view))]


@[ABWindow method:(appendOutput:(:id #|NSString|#)message)
           resultType:(:void)
           body:
           (let* ((output (output-view self))
                  (end    (unwrap (make-range :location [[output string] length]))))
             [output setSelectedRange:end]
             [output insertText:message]
             [output scrollRangeToVisible:end])]



@[ABWindow method:(keyDown:(:id #|NSEvent|#)event)
           resultType:(:void)
           body: (process-key-event event :window self)]



(defmethod format ((window ab-window) control-string &rest arguments)
  (let ((message (unwrap (apply (function cl:format) nil control-string arguments))))
    (on-main-thread [window appendOutput:message] :wait nil)))


;; (wrapping (wrap "Untitled"))

(defun paper-rect (paper-format)
  (ecase paper-format
    ((a4) (make-rect :x 0 :y 0 :width (* 72 210.0) :height (* 72 297.0)))))

(defun create-abwindow (rect &optional (title "Untitled AB Partition"))
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
         (command-rect   (make-rect :x 0 :y  0                      :width (rect-width rect) :height  32))
         (split-rect     (make-rect :x 0 :y (rect-top command-rect) :width (rect-width rect) :height (- (rect-height rect)
                                                                                                        (rect-top command-rect))))

         (output-rect    (make-rect :x 0 :y 0 :width (rect-width rect) :height 100))
         (partition-rect (paper-rect 'a4))
         (output-view    [[ABTextView  alloc] initWithFrame:(unwrap output-rect)])
         (command-view   [[ABTextField alloc] initWithFrame:(unwrap command-rect)])
         (partition-view [[ABView alloc] initWithFrame:(unwrap partition-rect)])
         (split-view     [[NSSplitView alloc] initWithFrame:(unwrap split-rect)]))
    (setf (output-view window) output-view
          (command-view window) command-view
          (partition-view window) partition-view)
    [split-view addSubview:partition-view]
    [split-view addSubview:output-view]
    [split-view adjustSubviews]
    [split-view setPosition:(- (rect-height split-rect) (rect-height output-rect)) ofDividerAtIndex:0]
    [window setTitle:title]
    [[window contentView] addSubview:split-view]
    [[window contentView] addSubview:command-view]
    [window setInitialFirstResponder:partition-view]
    (setf (partition partition-view) (partition window))
    (format window "Hello World!~%")
    window))




(defun initialize-keymaps ()
  (let ((dmap (create-keymap "global")))
    
    (let ((kmap (create-keymap "ABTextView" dmap)))
      
      )
    (let ((kmap (create-keymap "ABTextField" dmap)))
      
      )
    (let ((kmap (create-keymap "ABView" dmap)))
      
      )))

(initialize-keymaps)
