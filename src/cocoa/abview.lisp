(in-package "ABNOTATION.COCOA")
(objcl:set-objective-cl-syntax)


@[NSView subClass:ABView
         slots: ((partition         :initarg :partition
                                    :initform nil
                                    :accessor partition)
                 (cursor            :initarg :cursor
                                    :initform nil
                                    :accessor cursor)
                 (layout            :initform nil
                                    :accessor layout)
                 (paper-format      :initarg :paper-format
                                    :initform "A4"
                                    :accessor paper-format)
                 (paper-orientation :initarg :paper-orientation
                                    :initform :portrait
                                    :accessor paper-orientation)
                 (zoom              :initarg :zoom
                                    :initform 1.0
                                    :accessor zoom))]

@[ABView method:(drawRect:(:<nsr>ect)rect)
         resultType:(:void)
         body:(draw self (rect-from-macptr rect))]


(defmethod compute-frame-and-bounds ((view ab-view))
  (let ((bounds  (paper-bounds (paper-format view) (paper-orientation view)))
        (frame   (paper-frame view (paper-format view) (paper-orientation view) (zoom view))))
    (format [view window] "bounds=~S frame=~S~%" bounds frame)
    [view setFrame:(to-objc frame)]
    [view setBounds:(to-objc bounds)]
    [view setNeedsDisplay:YES])
  view)

(defmethod (setf zoom) :after (new-value (view ab-view))
  (compute-frame-and-bounds view))
(defmethod (setf paper-format) :after (new-value (view ab-view))
  (compute-frame-and-bounds view))
(defmethod (setf paper-orientation) :after (new-value (view ab-view))
  (compute-frame-and-bounds view))



(defmethod (setf partition) :after (partition (view ab-view))
  (if (cursor view)
      (unless (eql (partition cursor) partition)
        (setf (partition cursor) partition))
      (setf (cursor view) (make-instance 'cursor :partition partition))))

(defmethod page ((view ab-view))
  (page (cursor view)))

(defmethod draw ((view ab-view) &optional dirty-rect)
  (let ((*path-class* 'cocoa-bezier-path)
        (partition (partition view)))
    (when partition
      (let ((page (page view)))
        (when page
          (draw page dirty-rect))))))



;; (*path-class* 'cocoa-bezier-path)
;; (assert (typep p 'cocoa-bezier-path))

(defmethod fill-path ((path cocoa-bezier-path))
  [(bezier-path path) fill])

(defmethod stroke-path ((path cocoa-bezier-path))
  [(bezier-path path) stroke])



(defcommand zoom-in ()
  (setf (zoom (current-view)) (* (zoom (current-view)) 2)))

(defcommand zoom-out ()
  (setf (zoom (current-view)) (/ (zoom (current-view)) 2)))


(let ((abview-km (create-keymap "ABView" (global-keymap))))
  (keymap-set-key abview-km '((#\x :control) #\+) 'zoom-in)
  (keymap-set-key abview-km '((#\x :control) #\=) 'zoom-in)
  (keymap-set-key abview-km '((#\x :control) #\-) 'zoom-out)
  (setf (current-keymap) abview-km))

