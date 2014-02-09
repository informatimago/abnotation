(in-package "ABNOTATION.COMMAND")
(objcl:set-objective-cl-syntax)

(defun beep ()  (#_NSBeep))

(defvar *minibuffer* nil)
(defvar *message*    nil)

(defun message (control-string &rest arguments)
  (let ((message (format nil "~?" control-string arguments)))
    (format *trace-output* "~A~%" message)
    (format *message*      "~A~%" message)
    (format *minibuffer*   "~A"   message)))

(defparameter *modifier-map*
  (list (list #$NSAlphaShiftKeyMask  :alpha-shift "L-")
        (list #$NSShiftKeyMask       :shift       "S-")
        (list #$NSControlKeyMask     :control     "C-")
        (list #$NSAlternateKeyMask   :alternate   "A-")
        (list #$NSCommandKeyMask     :command     "c-")
        (list #$NSNumericPadKeyMask  :numeric-pad "n-")
        (list #$NSHelpKeyMask        :help        "h-")
        (list #$NSFunctionKeyMask    :function    "f-")))


;;;
;;; Key Chords
;;;

(defun modifier-string (modifier)
  (third (find modifier *modifier-map* :key (function second))))

(defun key-sequence-string (chords)
  (with-output-to-string (*standard-output*)
    (let ((sep ""))
     (dolist (chord chords)
       (princ sep) (setf sep " ")
       (if (consp chord)
         (progn
           (dolist (modifier (rest chord))
             (princ (modifier-string modifier)))
           (princ (first chord)))
         (princ chord))))))



(defun key (characters modifiers)
  (if modifiers
      (cons characters (sort modifiers (function string<)))
      characters))

(defun kbd (chord &rest chords)
  (mapcar (lambda (chord)
            (etypecase chord
              (character (key character '()))
              (string (when (< 1 (length chord))
                        (error "multi-character strings are not supported yet for kbd chords"))
                      (key (aref chord 0) '()))
              (cons
               (loop
                :with characters = nil
                :with modifiers = '()
                :for item :in chord
                :do (cond
                     ((stringp item)
                      (cond
                       (characters
                        (error "Too many characters in the chord ~S" chord))
                       ((< 1 (length item))
                        (error "multi-character strings are not supported yet for kbd chords"))
                       (t
                        (setf characters (aref item 0)))))
                     ((characterp item)
                      (if characters
                        (error "Too many characters in the chord ~S" chord)
                        (setf characters item)))
                     ((keywordp item)
                      (push item modifiers))
                     (t
                      (error "Invalid item ~S in chord ~S" item chord)))
                :finally (return (key characters modifiers))))))
          (cons chord chords)))

;; (kbd '(:control "c") "c") ((#\c :control) #\c)
;; (kbd "a" "b" "c")         (#\a #\b #\c)
;; (kbd "a")                 (#\a)
;; (kbd '(:control "a"))     ((#\a :control))



;;;
;;; KEYMAP
;;;
;;;  Maps CHORD (or character (cons character (list (member :shift :control :alternate :command))))
;;;  to commands.
;;;


(defvar *minimum-bindings-for-hash-table* 50)

(defclass keymap ()
  ((name     :initform nil :initarg :name   :accessor keymap-name)
   (parent   :initform nil :initarg :parent :accessor keymap-parent)
   (bindings :initform '() :initarg bindings
             :documentation "Bindings can be:
- a a-list, or
- a hash-table, or
- a function,
mapping key chords to commands.")))

(defgeneric keymap-raw-binding (keymap chord)
  (:method ((keymap null) chord)
           nil)
  (:method ((keymap keymap) chord)
           (let ((bindings (slot-value keymap 'bindings)))
             (etypecase bindings
               (list       (cdr (assoc chord bindings :test (function equal))))
               (hash-table (gethash chord bindings))
               (function   (funcall bindings chord))))))

(defun %keymap-upgrade-to-hash-table (keymap)
  (let ((bindings (slot-value keymap 'bindings)))
    (setf (slot-value keymap 'bindings)
          (map-into-hash-table bindings
                               :key (function car)
                               :value (function cdr)
                               :test (function equal)
                               :size (length bindings)))))

(defmethod (setf keymap-raw-binding) (new-command (keymap keymap) chord)
  (let ((bindings (slot-value keymap 'bindings)))
    (etypecase bindings
      (list       (if (<= *minimum-bindings-for-hash-table*
                          (length bindings))
                    (progn (%keymap-upgrade-to-hash-table keymap)
                           (setf (keymap-raw-binding keymap chord) new-command))
                    (let ((entry (assoc chord bindings :test (function equal))))
                      (cond
                       (entry
                        (setf (cdr entry) new-command))
                       (new-command
                        (push (cons chord new-command) (slot-value keymap 'bindings)))))))
      (hash-table (if new-command
                    (setf (gethash chord bindings) new-command)
                    (remhash chord bindings)))
      (function   (error "Cannot change bindings of a function keymap."))))
  new-command)


(defgeneric keymap-binding (keymap chord)
  (:method ((keymap null) chord)
           nil)
  (:method ((keymap keymap) chord)
           (or (keymap-raw-binding keymap chord)   
               (keymap-binding (keymap-parent keymap) chord))))





(defun define-key (keymap key command)
  (setf (keymap-raw-binding keymap key) command))


(defun keymap-set-key (keymap chords command)
  (let ((path   (butlast chords))
        (chord  (car (last chords)))
        (km     keymap)
        (parent (keymap-parent keymap)))
    (loop
     :for chord :in path
     :for next-km     = (keymap-raw-binding km chord)
     :for next-parent = (keymap-raw-binding parent chord)
     :do (progn
           (when (null next-km)
             (setf next-km (make-instance 'keymap :parent next-parent)
                   (keymap-raw-binding km chord) next-km))
           (setf km next-km
                 parent next-parent)))
    (setf (keymap-raw-binding km chord) command)))


(defun global-set-key (chords command)
  (keymap-set-key (global-keymap) chords command))


;;;
;;;
;;;


(defvar *keymaps*
  (make-hash-table :test (function equal))
  "Maps class names (strings) to keymaps.")

(defun create-keymap (name &optional parent)
  "Creates a new named keymap."
  (setf (gethash name *keymaps*)
        (make-instance 'keymap :name name :parent parent)))
 
(defun keymap-named (name)
   (gethash name *keymaps*))

(defun global-keymap ()
  (or (keymap-named "global") (create-keymap "global")))

(defun get-keymap (name)
  "Return the keymap named NAME, or if none, the global keymap."
  (or (gethash name *keymaps*)
      (global-keymap)))


;;;
;;; INPUT STATE
;;;


(defclass input-state ()
  ((current-keymap :initarg :keymap :accessor input-state-current-keymap)
   (modal-keymap :initform nil)
   (so-far :initform '())))

(defmethod input-state-modal-keymap ((state input-state))
  (or (slot-value state 'modal-keymap)
      (input-state-current-keymap state)))

(defmethod (setf input-state-current-keymap) :after (keymap (state input-state))
  (setf (slot-value state 'modal-keymap) nil))

(defmethod reset-modal-keymap ((state input-state))
  (setf (slot-value state 'modal-keymap) nil))
(defmethod shift-modal-keymap ((state input-state) (keymap keymap))
  (setf (slot-value state 'modal-keymap) keymap))


(defparameter *input-state* (make-instance 'input-state :keymap (global-keymap)))

(defun current-keymap ()
  (input-state-current-keymap *input-state*))

(defun (setf current-keymap) (keymap)
  (setf (input-state-current-keymap *input-state*) keymap))

(defun key-binding (chord)
  (keymap-binding (current-keymap) chord))


(defvar *debug-on-error* nil "Non-nil means enter debugger if an error is signaled.")

(defmethod call-interactively ((state input-state) command)
  (handler-bind
      ((error (lambda (err)
                (message "~A" err)
                (when *debug-on-error*
                  (invoke-debugger err))
                (throw :petite-gazongue 0))))
    (funcall command)))

(defmethod process-key ((state input-state) chord)
  (let* ((keymap  (input-state-modal-keymap state))
         (command (keymap-binding keymap chord)))
    (with-slots (so-far) state
      (push chord so-far)
      (message (key-sequence-string (reverse so-far)))
      (etypecase command
        (null     (beep)
                  (setf so-far '())
                  (reset-modal-keymap state)
                  (message ""))
        ;; (string "keyboard-macro")
        (keymap   (shift-modal-keymap state command))
        (symbol   (setf so-far '())
                  (message "~(~A~)" command)
                  (reset-modal-keymap state)
                  (call-interactively state command))
        (function (setf so-far '())
                  (message "")
                  (reset-modal-keymap state)
                  (call-interactively state command))))))


(defvar *current-window* nil)
(defun current-window () *current-window*)
(defvar *current-view* nil)
(defun current-view () *current-view*)
  

(defmacro defcommand (name lambda-list &body body)
  `(defun ,name ,lambda-list ,@body))





;; (process-key *input-state* '(#\x :alternate))
;; (process-key *input-state* '#\-)
;; (inspect *input-state*)



;; (chord "a"'(
;;             :alpha-shift  ; L-
;;             :shift        ; S-
;;             :control      ; C-
;;             :alternate    ; A-
;;             :command      ; c-
;;             :numeric-pad  ; n-
;;             :help         ; h-
;;             :function     ; F-
;;             ))
;; ("a" :alpha-shift :alternate :command :control :function :help :numeric-pad :shift)



;; (defun process-key-event (event &key window view)
;;   (catch :petite-gazongue
;;     (let* ((characters (objcl:lisp-string [event charactersIgnoringModifiers]))
;;            (modifiers (loop
;;                        :with modifiers = [event modifierFlags]
;;                        :for (mask modifier) :in *modifier-map*
;;                        :unless (zerop (logand mask modifiers))
;;                        :collect modifier))
;;            (window  (or window [view window]))
;;            (command (keymap-binding 
;;                      (get-keymap (objcl:lisp-string [(or window view) className]))
;;                      (key characters modifiers))))
;;       (format window "Characters: ~S Modifiers: (~{~S~^ ~})~%" characters modifiers)
;;       (if command
;;         (funcall command)
;;         (beep)))))

(defun process-key-event (event &key window view)
  (catch :petite-gazongue
    (let* ((character (aref (objcl:lisp-string [event charactersIgnoringModifiers]) 0))
           (modifiers (remove :shift
                              (loop
                               :with modifiers = [event modifierFlags]
                               :for (mask modifier) :in *modifier-map*
                               :unless (zerop (logand mask modifiers))
                               :collect modifier))))
      (process-key *input-state* (key character modifiers)))
    (return-from process-key-event))
  (message "Cancelled"))


;;;; THE END ;;;;
