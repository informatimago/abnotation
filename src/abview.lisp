(in-package "ABNOTATION")
(objcl:set-objective-cl-syntax)

@[NSView subClass:ABView
         slots: ((partition :initform nil :accessor partition))]

@[ABView method:(drawRect:(:<NSR>ect)rect)
         resultType:(:void)
         body:(draw-rect partition rect)]

