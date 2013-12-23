(in-package "ABNOTATION.COMMAND")
(objcl:set-objective-cl-syntax)


(defparameter *modifier-map*
  (list (cons #$NSAlphaShiftKeyMask  :alpha-shift)
        (cons #$NSShiftKeyMask       :shift)
        (cons #$NSControlKeyMask     :control)
        (cons #$NSAlternateKeyMask   :alternate)
        (cons #$NSCommandKeyMask     :command)
        (cons #$NSNumericPadKeyMask  :numeric-pad)
        (cons #$NSHelpKeyMask        :help)
        (cons #$NSFunctionKeyMask    :function)))

(defclass keymap ()
  ((name :initarg :name :accessor keymap-name)
   (parent :initarg :parent :accessor keymap-parent)
   (bindings :initarg :bindings :initform (make-hash-table :test (function equal)) :reader bindings
             :documentation "bindings can be a hash-table or a function mapping key chords to commands.")))


(defvar *current-keymap* nil)

(defun key-binding (key &optional (keymap *current-keymap*))
  (when keymap
    (let ((bindings  (bindings keymap)))
      (or (etypecase bindings
            (function (funcall bindings key))
            (hash-table (gethash key binding)))
          (key-binding key (keymap-parent keymap))))))


(defvar *keymaps*
  (make-hash-table :test (function equal))
  "Maps class names (strings) to keymaps.")

(defun create-keymap (class-name &optional parent)
  (setf (gethash class-name *keymaps*)
        (or (gethash class-name *keymaps*)
            (make-instance 'keymap :name class-name :parent parent))))

(defun current-global-map ()
  (gethash "global" *keymaps*))

(defun get-keymap (class-name)
  (or (gethash class-name *keymaps*)
      (current-global-map)))

(defun define-key (keymap key command)
  (if (null command)
      (remhash key keymap)
      (setf (gethash key keymap) command)))


(defun key (characters modifiers)
  (if modifiers
      (cons characters (sort modifiers (function string<)))
      characters))

(defgeneric kbd (chord)
  (:method ((chord string))
    (error "Not implemented yet."))
  (:method ((chord cons))
    (loop
      :with characters = nil
      :with modifiers = '()
      :for item :in chord
      :do (cond
            ((stringp item) (if characters
                                (error "Too many characters in the chord ~S" chord)
                                (setf characters item)))
            ((characterp item) (if characters
                                   (error "Too many characters in the chord ~S" chord)
                                   (setf characters (string item))))
            ((keywordp item) (push item modifiers))
            (t (error "Invalid item ~S in chord ~S" item chord)))
      :finally (return (key characters modifiers)))))


(defun global-set-key (key command)
  (define-key (current-global-map) key command))


;; (chord "a"'(
;;             :alpha-shift
;;             :shift     
;;             :control   
;;             :alternate 
;;             :command   
;;             :numeric-pad
;;             :help      
;;             :function
;;             ))
;; ("a" :alpha-shift :alternate :command :control :function :help :numeric-pad :shift)

(defun process-key-event (event &key window view)
 (let ((characters (objcl:lisp-string [event charactersIgnoringModifiers]))
       (modifiers (loop
                    :with modifiers = [event modifierFlags]
                              :for (mask . modifier) :in *modifier-map*
                              :unless (zerop (logand mask modifiers))
                              :collect modifier))
       (window  (or window [view window]))
       (command (key-binding (key characters modifiers)
                             (get-keymap (objcl:lisp-string [(or window view) className])))))
   (format window "Characters: ~S Modifiers: (~{~S~^ ~})~%" characters modifiers)
   (if command
     (funcall command)
     (#_NSBeep))))


;;;; THE END ;;;;
