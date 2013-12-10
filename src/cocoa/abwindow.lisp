(in-package "ABNOTATION")
(objcl:set-objective-cl-syntax)

(defgeneric format (output control-string &rest arguments )
  (:method ((output null) control-string &rest arguments)
    (apply (function cl:format) output control-string arguments))
  (:method ((output stream) control-string &rest arguments)
    (apply (function cl:format) output control-string arguments)))


@[NSWindow subClass:ABWindow
         slots: ((partition :initform nil :accessor partition-view)
                 (command   :initform nil :accessor command-view)
                 (output    :initform nil :accessor output-view))]


@[ABWindow method:(appendOutput:(:id)message)
           resultType:(:void)
           body:
           (let* ((output (output-view self))
                  (end    (unwrap (make-range :location [[output string] length]))))
             [output setSelectedRange:end]
             [output insertText:message]
             [output scrollRangeToVisible:end])]


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
         (output-view    [[NSTextView  alloc] initWithFrame:(unwrap output-rect)])
         (command-view   [[NSTextField alloc] initWithFrame:(unwrap command-rect)])
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
    (format window "Hello World!~%")
    window))






