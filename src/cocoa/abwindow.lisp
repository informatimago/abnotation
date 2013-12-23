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


@[NSWindow subClass:ABWindow
           slots: ((partition        :initform (create-partition *staves/bass15mb-trebble15ma*)
                                     :accessor partition)
                   (partition-subview   :initform nil :accessor partition-subview)
                   (scroll-subview      :initform nil :accessor scroll-subview)
                   (command-subview     :initform nil :accessor command-subview)
                   (output-subview      :initform nil :accessor output-subview))]


@[ABWindow method:(appendOutput:(:id #|NSString|#)message)
           resultType:(:void)
           body:
           (let* ((output (output-subview self))
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
         (scroll-rect    (make-rect :x 0 :y (height output-rect)
                                    :width (width rect) :height (- (height split-rect) (height output-rect))))
         (output-subview    [[ABTextView  alloc] initWithFrame:(unwrap output-rect)])
         (command-subview   [[ABTextField alloc] initWithFrame:(unwrap command-rect)])
         (partition-subview [[ABView alloc] initWithFrame:(unwrap scroll-rect)])
         (split-subview     [[NSSplitView alloc] initWithFrame:(unwrap split-rect)])
         (scroll-subview    (scroll-view scroll-rect partition-subview)))
    (setf (output-subview window)    output-subview
          (command-subview window)   command-subview
          (partition-subview window) partition-subview
          (scroll-subview window)    scroll-subview)
    [split-subview addSubview:scroll-subview]
    [split-subview addSubview:output-subview]
    [split-subview adjustSubviews]
    [split-subview setPosition:(- (rect-height split-rect) (rect-height output-rect)) ofDividerAtIndex:0]
    [window setTitle:title]
    [[window contentView] addSubview:split-subview]
    [[window contentView] addSubview:command-subview]
    [window setInitialFirstResponder:partition-subview]
    (compute-frame-and-bounds partition-subview)
    (setf (partition partition-subview) (partition window))
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
