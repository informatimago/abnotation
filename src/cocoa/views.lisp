;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               views.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Subclasses of NS views.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-12-16 <PJB> Created.
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

@[NSTextView subClass:ABTextView slots:()]

(defvar *text-font-size* 12.0)
@[ABTextView method:(initWithFrame:(:<NSR>ect)frame)
             resultType:(:id)
             body:(let ((self [super initWithFrame:frame]))
                    (unless (oclo:nullp self)
                      [self setFont:[NSFont userFixedPitchFontOfSize:*text-font-size*]])
                    self)]


@[ABTextView method:(keyDown:(:id)event)
             resultType:(:void)
             body:(process-key-event event :view self)]

@[NSTextField subClass:ABTextField slots:()]

@[ABTextField method:(keyDown:(:id)event)
              resultType:(:void)
              body:(process-key-event event :view self)]


(defun scroll-view (frame &optional document-view)
  (let ((scroll-view [[NSScrollView alloc] initWithFrame:(unwrap frame)]))
    [scroll-view setHasVerticalScroller:YES]
    [scroll-view setHasHorizontalScroller:YES]
    [scroll-view setBorderType:#$NSLineBorder]
    [scroll-view setAutoresizingMask:(logior #$NSViewWidthSizable
                                             #$NSViewHeightSizable)]
    (when document-view
      [scroll-view setDocumentView:document-view]) 
    scroll-view))


(defun normalize-split-subviews (ratio-and-subviews total-height)
  (loop
   :with subviews = '()
   :with sum = 0
   :with unspecified = 0
   :while ratio-and-subviews
   :do (let ((item (pop ratio-and-subviews)))
         (if (typep item '(real 0))
           (let ((view (pop ratio-and-subviews)))
             (incf sum item)
             (push (cons item view) subviews))
           (progn
             (incf unspecified)
             (push (cons nil item) subviews))))
   :finally
   (when (<= sum 1)
     (dolist (entry subviews)
       (when (car entry)
         (setf (car entry) (* (car entry) total-height))))
     (setf sum (* sum total-height)))
   (when (plusp unspecified)
     (let ((unspecified-height (/ (- total-height sum) unspecified)))
       (dolist (entry subviews)
         (unless (car entry)
           (setf (car entry) unspecified-height)))))
   (return (nreverse subviews))))

(defun test/normalize-split-subviews ()
  (assert (equal (normalize-split-subviews '(0.10 a 0.20 b c d) 100)
                 '((10.0 . a) (20.0 . b) (35.0 . c) (35.0 . d))))
  (assert (equal (normalize-split-subviews '(10 a 20 b c d) 100)
                 '((10 . a) (20 . b) (35 . c) (35 . d)))))


(defun split-view (frame ratio-and-subviews
                         &key (direction :horizontal) (divider-style :thick))
  "
FRAME:               a RECT specifying the frame of the new NSSplitView.

DIRECTION:           :vertical or :horizontal (:horizontal means the
                     subviews are one above the other, the splitters
                     are horizontal).

DIVIDER-STYLE:       :thick :thin :pane-spliter divider style.

RATIO-AND-SUBVIEWS:  a list of NSViews. Each view can optionnaly be
                     prefixed by a (REAL 0).

                     If the sum of the reals is greater than 1 and
                     less than the frame height, then they're taken as heigths.

                     If the sum is less or equal to 1 then they're
                     taken as percentage.

                     When there are missing reals, the remaining space
                     or proportion is evenly spread over the views
                     that don't have one.

RETURN:              A new NSSplitView containing the subviews.
"
  (check-type direction (member :horizontal :vertical))
  (check-type divider-style (member :thin :thick :pane-spliter))
  (let* ((subviews (normalize-split-subviews ratio-and-subviews
                                             (if (eq direction :horizontal)
                                               (height frame)
                                               (width frame))))
         (split-view [[NSSplitView alloc] initWithFrame:(to-objc frame)]))
    [split-view setVertical:(if (eq direction :horizontal) NO YES)]
    [split-view setDividerStyle:(case divider-style
                                  (:thin #$NSSplitViewDividerStyleThin)
                                  (:thick #$NSSplitViewDividerStyleThick)
                                  (:pane-spliter #$NSSplitViewDividerStylePaneSplitter))]
    (dolist (subview subviews)
      [split-view addSubview:(cdr subview)])
    [split-view adjustSubviews]
    (loop
     :for ((height . nil) . not-last-one) :on subviews
     :for y = height :then (+ y height)
     :for i :from 0
     :when not-last-one
     :do [split-view setPosition:y ofDividerAtIndex:i])
    [split-view setAutoresizingMask:(logior #$NSViewWidthSizable
                                            #$NSViewHeightSizable)]
    split-view))



(defgeneric device-resolution (view-or-window)
  (:documentation "Return the device resolution in dot/inch.")
  (:method ((window ns:ns-window))
           (get-nssize [[[window deviceDescription] objectForKey:#$NSDeviceResolution] sizeValue]))
  (:method ((view ns:ns-view))
           (device-resolution [view window])))


(defgeneric dot/inch->dot/mm (resolution)
  (:method ((resolution real))
           (/ resolution 25.4))
  (:method ((resolution size))
           (size (/ (size-width resolution) 25.4)
                 (/ (size-height resolution) 25.4))))


(defun paper-bounds  (paper-format paper-orientation)
  "Return the bounds for a view showing a page of the given paper format and orientation, in millimeters."
  (destructuring-bind (width height)(paper-size-and-printable-area paper-format paper-orientation)
    (make-rect :x 0.0 :y 0.0 :width width :height height)))

(defun paper-frame (view-or-window  paper-format paper-orientation &optional (zoom 1.0))
  "Return the frame for a view showing a page of the given paper format and orientation, in view-or-windows coordinates."
  (let* ((size       (paper-size-and-printable-area paper-format paper-orientation))
         (resolution (dot/inch->dot/mm (device-resolution view-or-window)))
         (frame      (make-rect :x 0.0 :y 0.0
                                :width  (* zoom (first  size) (size-width  resolution))
                                :height (* zoom (second size) (size-height resolution)))))
    (format *trace-output* "resolution = ~S dot/mm~%" resolution)
    (format *trace-output* "paper-size = ~S mm~%" size)
    (format *trace-output* "paper-frame = ~S dot~%" frame)
    frame))



;; (paper-bounds "A4" :portrait)
;; (paper-size "A4" :portrait)
;; (list (paper-bounds "A4" :portrait)
;;       (paper-frame *window* "A4" :portrait)
;;       (paper-frame *window* "A4" :portrait 2.0))
;; (#S(rect :x 0.0D0 :y 0.0D0 :width 210.0D0 :height 297.0D0)
;;    #S(rect :x 0.0D0 :y 0.0D0 :width 595.2755994913236D0 :height 841.8897764234434D0)
;;    #S(rect :x 0.0D0 :y 0.0D0 :width 1190.5511989826473D0 :height 1683.7795528468869D0))
;; (dot/inch->dot/mm (device-resolution *window*))
;; #S(size :width 2.834645711863446D0 :height 2.834645711863446D0)

;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (format t "~&processing views.lisp~%"))

;;;; THE END ;;;;
