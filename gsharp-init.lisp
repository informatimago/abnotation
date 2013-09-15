(in-package :gsharp)


(set-key `(com-erase-element ,*numeric-argument-marker*)
         'global-gsharp-table
         '((#\Backspace)))


(set-key `(esa:com-describe-bindings ,*numeric-argument-marker*)
         'global-gsharp-table
         '((#\h :control) #\m))




(define-gsharp-command (com-pjb :name t)
    ((argument 't :prompt "prefix"))
  (format *trace-output* "~S argument = ~S~%" 'com-pjb argument))

(set-key `(com-pjb ,*numeric-argument-marker*)
         'global-gsharp-table
         '((#\p)))



(define-gsharp-command (com-set-key :name t)
    ((command 'command :prompt "Command")
     (key     'list    :prompt "Key"))
  (set-key `(,command)
           'global-gsharp-table
           key))





;;(clim:find-command-table 'gsharp::cluster-table :errorp t)


(in-package :cl-user)
(defpackage :clim-backend-text-stream
  (:use :cl)
  (:export :define-stream-methods
           :object-set-attribute
           :object-get-attribute))
(in-package :clim-backend-text-stream)

(defvar *object-attributes* (make-hash-table)); TODO: use a weak hash-table

(defmethod object-set-attribute (object attribute value)
  (setf (getf (gethash object *object-attributes*) attribute) value))

(defmethod object-get-attribute (object attribute &optional default)
  (getf (gethash object *object-attributes*) attribute default))

;; (object-get-attribute 1 'name "One")
;; (object-get-attribute 1 'name)
;; (object-set-attribute 1 'name "One")
;; (object-set-attribute 1 'french "Un")
;; (object-get-attribute 1 'french)

(defmacro define-stream-methods (class-name slots &rest options)
  (let ((defaults (cdr (assoc :default-initargs options))))
    `(progn
       ,@(mapcan
          (lambda (slot-def)
              (destructuring-bind (name &key accessor reader
                                        (initarg nil initargp)
                                        (initform nil initformp)
                                        &allow-other-keys) slot-def
                (append
                 (when accessor
                   (list
                    `(defmethod ,accessor ((self ,class-name))
                       (object-get-attribute self ',name ,(cond
                                                           (initargp (getf defaults initarg))
                                                           (initformp initform))))
                    `(defmethod (setf ,accessor) (new-value (self ,class-name))
                       (object-set-attribute self ',name new-value))))
                 (when reader
                   (list
                    `(defmethod ,reader ((self ,class-name))
                       (object-get-attribute self ',name ,(cond
                                                           (initargp (getf defaults initarg))
                                                           (initformp initform)))))))))
          slots))))




(in-package :clim-internals)

(clim-backend-text-stream:define-stream-methods cl:stream
    ((cursor :accessor stream-text-cursor)
     (foreground :initarg :foreground :reader stream-foreground)
     (background :initarg :background :reader stream-background)
     (text-style :initarg :text-style :reader stream-text-style)
     (vspace :initarg :vertical-spacing :reader stream-vertical-spacing)
     (margin :initarg :text-margin :writer (setf stream-text-margin))
     (eol :initarg :end-of-line-action :accessor stream-end-of-line-action)
     (eop :initarg :end-of-page-action :accessor stream-end-of-page-action)
     (view :initarg :default-view :accessor stream-default-view)
     (baseline :initform 0 :reader stream-baseline)
     ;; What is this? --GB
     (height :initform 0)
     ;; When the stream takes part in the space alloction protocol, this
     ;; remembers our demand:
     (seos-current-width  :initform 0)
     (seos-current-height :initform 0)
     (buffering-output :accessor medium-buffering-output-p))
  (:default-initargs
      :foreground +black+ :background +white+ :text-style *default-text-style*
      :vertical-spacing 2 :text-margin nil :end-of-line-action :wrap
      :end-of-page-action :scroll :default-view +textual-view+))


;;; 11.1.1 Text Style Protocol and Text Style Suboptions

;; (defmethod text-style-ascent (text-style (medium cl:stream)))
;; (defmethod text-style-descent (text-style (medium cl:stream)))
;; (defmethod text-style-height (text-style (medium cl:stream)))
;; (defmethod text-style-width (text-style (medium cl:stream)))
;; (defmethod text-style-fixed-width-p (text-style (medium cl:stream)))

(defmethod text-size ((medium cl:stream) string &key text-style start end)
  (declare (ignore text-style))
  (let ((string (string string)))
    (- (or (and end (min end (length string)))
           (length string))
       (or start 0))))



;;; 15.3.2 Stream Text Cursor Protocol [complete]

(defmethod clim-backend-text-stream:object-get-attribute
    :before ((medium cl:stream) (attribute (eql 'cursor)) &optional default)
  (multiple-value-bind (attributes presentp) (gethash object *object-attributes*)
    (declare (ignore attributes))
    (unless presentp
      (setf (stream-text-cursor medium) (make-instance 'standard-text-cursor :sheet stream))
      (setf (cursor-active (stream-text-cursor stream)) t))))


(defmethod stream-cursor-position ((medium cl:stream))
  (cursor-position (stream-text-cursor medium)))

(defmethod (setf stream-cursor-position) (x y (medium cl:stream))
  (setf (cursor-position (stream-text-cursor medium)) (values x y)))

(defmethod stream-set-cursor-position ((medium cl:stream) x y)
  (setf (stream-cursor-position medium) (values x y)))

(defmethod stream-increment-cursor-position ((medium cl:stream) dx dy)
  (multiple-value-bind (x y) (cursor-position (stream-text-cursor medium))
    (let ((dx (or dx 0))
	  (dy (or dy 0)))
    (setf (cursor-position (stream-text-cursor medium)) (values (+ x dx) (+ y dy))))))


;;; 15.4 Text Protocol [complete]

(defmethod stream-character-width ((medium cl:stream) character &key text-style)
  (declare (ignore text-style))
  1)

(defmethod stream-string-width ((medium cl:stream) string &key start end text-style)
  (declare (ignore text-style))
  (let ((string (string string)))
    (- (or (and end (min end (length string)))
           (length string))
       (or start 0))))

(defmethod stream-line-height ((medium cl:stream) &key text-style)
  (declare (ignore text-style))
  1)

(defmethod stream-vertical-spacing ((medium cl:stream))
  1)

(defmethod stream-baseline ((medium cl:stream))
  0)


;;; 16.4.1. The Output Recording (medium cl:stream) Protocol

(defmethod stream-recording-p ((medium cl:stream)) nil)
(defmethod stream-drawing-p   ((medium cl:stream)) nil)


;; (defmethod (setf stream-recording-p) (recording-p (medium cl:stream)))
;; (defmethod (setf stream-drawing-p) (drawing-p (medium cl:stream)))
;; 
;; (defmethod stream-output-history ((medium cl:stream)))
;; (defmethod stream-current-output-record ((medium cl:stream)))
;; (defmethod (setf stream-current-output-record) (record (medium cl:stream)))
;; (defmethod stream-add-output-record ((medium cl:stream) record))
;; (defmethod stream-replay ((medium cl:stream) &optional region))
;; (defmethod erase-output-record (record (medium cl:stream) &optional errorp))

;;; 16.4.3. Text Output Recording

;; (defmethod stream-text-output-record ((medium cl:stream) text-style))
;; 
;; (defmethod stream-close-text-output-record ((medium cl:stream))
;;   (let ((record (clim:medium-current-text-output-record medium)))
;;     (when record
;;       (setf (clim-internals::stream-current-text-output-record medium) nil)
;;       (clim-internals::stream-add-output-record medium record))))
;; 
;; (defmethod stream-add-character-output ((medium cl:stream) character text-style width height baseline))
;; (defmethod stream-add-string-output ((medium cl:stream) string start end text-style width height baseline))
;; 
;; (defvar *output-records* (make-hash-table)
;;   "Maps streams to list of records.")
;; 
;; 
;; 
;; (defvar *current-output-records* (make-hash-table)
;;   "Maps streams to records.")
;; 
;; (defmethod clim-internals::stream-current-text-output-record  ((stream cl:stream))
;;   (gethash stream *current-output-records*))
;; 
;; (defmethod (setf clim-internals::stream-current-text-output-record)  (new-record (stream cl:stream))
;;   (if (null new-record)
;;     (remhash stream *current-output-records*)
;;     (setf (gethash stream *current-output-records*) new-record)))
;; 
;; 
;; 
;; (defmethod invoke-with-new-output-record ((medium cl:strean) continuation record-type constructor &key &allow-other-keys)
;;   (declare (ignore record-type))
;;   (stream-close-text-output-record stream)
;;   (let ((new-record (funcall constructor)))
;;     (letf (((clim:stream-current-output-record stream) new-record))
;;       ;; Should we switch on recording? -- APD
;;       (funcall continuation stream new-record)
;;       (force-output stream))
;;     (if parent
;;       (add-output-record new-record parent)
;;       (stream-add-output-record stream new-record))
;;     new-record))



#-(and)
(let ((*application-frame* (first *gsharp-instances*)))
  (esa::describe-bindings *standard-output*
                          (esa::find-applicable-command-table *application-frame*)
                          #'esa::sort-by-keystrokes))
