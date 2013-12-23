(in-package "ABNOTATION.COCOA")
(objcl:set-objective-cl-syntax)


@[NSView subClass:ABView
         slots: ((partition :initarg :partition
                            :initform nil
                            :accessor partition)
                 (page      :initform nil
                            :accessor page)
                 (layout    :initform nil
                            :accessor layout))]

@[ABView method:(drawRect:(:<NSR>ect)rect)
         resultType:(:void)
         body:(draw-partition-page self rect)]

(defmethod (setf partition) :after (partition (self ab-view))
  (setf (page self) (elt (pages partition) 0)))


(defun draw-partition-page (abview rect)
  (let* ((*path-class* 'cocoa-bezier-path)
         (p (create-path)))
    (assert (typep p 'cocoa-bezier-path))
    (move-to-coordinates p nil 100 100)
    (curve-to-coordinates p nil 100 200 200 200 200 100 )
    (close-subpath p)
    [(bezier-path p) stroke])
  (draw (rect 100 100 200 200))
  (values))


(defmethod draw ((r rect))
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



