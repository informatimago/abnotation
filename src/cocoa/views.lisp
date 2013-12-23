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
    (when document-view
      [scroll-view setDocumentView:document-view]) 
    scroll-view))



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
  (make-rect :x 0.0 :y 0.0 :size (apply (function size) (paper-size-and-printable-area paper-format paper-orientation))))

(defun paper-frame (view-or-window  paper-format paper-orientation &optional (zoom 1.0))
  "Return the frame for a view showing a page of the given paper format and orientation, in view-or-windows coordinates."
  (let ((size       (paper-size-and-printable-area paper-format paper-orientation))
        (resolution (dot/inch->dot/mm (device-resolution view-or-window))))
    (make-rect :x 0.0 :y 0.0
               :width  (* zoom (first  size) (size-width  resolution))
               :height (* zoom (second size) (size-height resolution)))))



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
