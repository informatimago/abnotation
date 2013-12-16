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
  (let* ((*path-class* 'ns-bezier-path)
         (p (create-path)))
    (move-to-coordinates p 100 100)
    (curve-to-coordinates p 100 200 200 200 200 100 )
    (close-subpath p)
    [(bezier-path p) stroke])
  (values))
