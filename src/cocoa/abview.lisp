(in-package "ABNOTATION.COCOA")
(objcl:set-objective-cl-syntax)


@[NSView subClass:ABView
         slots: ((partition         :initarg :partition
                                    :initform nil
                                    :accessor partition)
                 (page              :initform nil
                                    :accessor page)
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
    [view setBounds:(to-objc bounds)]
    [view setFrame:(to-objc frame)])
  view)

(defmethod (setf zoom) :after (new-value (view ab-view))
  (compute-frame-and-bounds view))
(defmethod (setf paper-format) :after (new-value (view ab-view))
  (compute-frame-and-bounds view))
(defmethod (setf paper-orientation) :after (new-value (view ab-view))
  (compute-frame-and-bounds view))



(defmethod (setf partition) :after (partition (self ab-view))
  (setf (page self) (elt (pages partition) 0)))


(defmethod draw ((view ab-view) &optional dirty-rect)
  (let ((*path-class* 'cocoa-bezier-path)
        (partition (partition view)))
    (when partition
      (let ((page (page view)))
        (when page
          (draw page dirty-rect)))))
  (let* ((*path-class* 'cocoa-bezier-path)
         (p (create-path)))
    (assert (typep p 'cocoa-bezier-path))
    (move-to-coordinates p nil 100 100)
    (curve-to-coordinates p nil 100 200 200 200 200 100 )
    (close-subpath p)
    [(bezier-path p) stroke])
  (draw (rect 100 100 200 200))
  (values))


(defmethod draw ((r rect) &optional clip-rect)
  (let* ((*path-class* 'cocoa-bezier-path)
         (p      (create-path))
         (left   (rect-left   r))
         (right  (rect-right  r))
         (top    (rect-top    r))
         (bottom (rect-bottom r)))
    (assert (typep p 'cocoa-bezier-path))
    (move-to-coordinates p nil left bottom)
    (line-to-coordinates p nil left top)
    (line-to-coordinates p nil right top)
    (line-to-coordinates p nil right bottom)
    (line-to-coordinates p nil left bottom)
    (move-to-coordinates p nil (1+ left) (1+ bottom))
    (line-to-coordinates p nil (1- right) (1+ bottom))
    (line-to-coordinates p nil (1- right) (1- top))
    (line-to-coordinates p nil (1+ left) (1- top))
    (line-to-coordinates p nil (1+ left) (1+ bottom))
    (close-subpath p)
    [(bezier-path p) fill]))



