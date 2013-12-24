(in-package "ABNOTATION.COMMAND")
(objcl:set-objective-cl-syntax)

(defun beep ()  (#_NSBeep))

(defparameter *modifier-map*
  (list (cons #$NSAlphaShiftKeyMask  :alpha-shift)
        (cons #$NSShiftKeyMask       :shift)
        (cons #$NSControlKeyMask     :control)
        (cons #$NSAlternateKeyMask   :alternate)
        (cons #$NSCommandKeyMask     :command)
        (cons #$NSNumericPadKeyMask  :numeric-pad)
        (cons #$NSHelpKeyMask        :help)
        (cons #$NSFunctionKeyMask    :function)))


;;;
;;; Key Chords
;;;

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

(defgeneric keymap-binding (keymap chord)
  (:method ((keymap null) chord)
           nil)
  (:method ((keymap keymap) chord)
           (let* ((bindings (slot-value keymap 'bindings))
                  (command  (etypecase bindings
                              (list       (cdr (assoc chord bindings :test (function equal))))
                              (hash-table (gethash chord bindings))
                              (function   (funcall bindings chord)))))
             (or command (keymap-binding (keymap-parent keymap) chord)))))


(defmethod (setf keymap-bindings) (new-bindings (keymap keymap))
  (setf (slot-value keymap 'bindings) new-bindings))


(defun %keymap-upgrade-to-hash-table (keymap)
  (let ((bindings (slot-value keymap 'bindings)))
    (setf (slot-value keymap 'bindings)
          (map-into-hash-table bindings
                               :key (function car)
                               :value (function cdr)
                               :test (function equal)
                               :size (length bindings)))))

(defmethod (setf keymap-binding) (new-command (keymap keymap) chord)
  (let ((bindings (slot-value keymap 'bindings)))
    (etypecase bindings
      (list       (if (<= *minimum-bindings-for-hash-table*
                          (length bindings))
                    (progn (%keymap-upgrade-to-hash-table keymap)
                           (setf (keymap-binding keymap chord) new-command))
                    (let ((entry (assoc chord bindings :test (function equal))))
                      (cond
                       (entry
                        (setf (cdr entry) new-command))
                       (new-command
                        (push (cons chord new-command) (slot-value keymap 'bindings)))))))
      (hash-table (if new-command
                    (setf (gethash chord bindings) new-command)
                    (remash chord bindings)))
      (function   (error "Cannot change bindings of a function keymap.")))))


(defun define-key (keymap key command)
  (setf (keymap-binding keymap key) command))

(defun global-set-key (key command)
  (define-key (global-keymap) key command))


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
   (modal-keymap :initform nil)))

(defmethod input-state-modal-keymap ((state input-state))
  (or (slot-value state 'modal-keymap)
      (input-state-current-keymap state)))

(defmethod shift-modal-keymap ((state input-state) (keymap keymap))
  (setf (slot-value state 'modal-keymap) keymap))


(defparameter *input-state* (make-instance 'input-state :keymap (global-keymap)))

(defun current-keymap ()
  (input-state-current-keymap *input-state*))

(defun key-binding (chord)
  (keymap-binding (current-keymap) chord))


(defmethod call-interactively ((state input-state) command)
  (funcall command))

(defmethod process-key ((state input-state) chord)
  (let* ((keymap  (input-state-modal-keymap state))
         (command (keymap-binding keymap chord)))
    (etypecase command
      (null   (beep))
      ;; (string "keyboard-macro")
      (keymap (shift-modal-keymap state command))
      (symbol (call-interactively state command)))))



(defmacro command (lambda-list &body body)
  `(lambda ,lambda-list body))

(let ((abview-km (create-keymap "ABView" (global-keymap)))
      (abview-A-x-km (create-keymap "ABView A-x")))

  (define-key abview-A-x-km '(#\x :alternate)
    abview-A-x-km)
  
  (define-key abview-A-x-km #\+
    (command () (setf (zoom *abview*) (* (zoom *abview*) 2))))

  (define-key abview-A-x-km #\-
    (command () (setf (zoom *abview*) (/ (zoom *abview*) 2)))))

;; (process-key *input-state* '(#\x :alternate))
;; (process-key *input-state* '#\+)





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



(defun process-key-event (event &key window view)
  (catch :petite-gazongue
    (let* ((characters (objcl:lisp-string [event charactersIgnoringModifiers]))
           (modifiers (loop
                       :with modifiers = [event modifierFlags]
                       :for (mask . modifier) :in *modifier-map*
                       :unless (zerop (logand mask modifiers))
                       :collect modifier))
           (window  (or window [view window]))
           (command (keymap-binding 
                     (get-keymap (objcl:lisp-string [(or window view) className]))
                     (key characters modifiers))))
      (format window "Characters: ~S Modifiers: (~{~S~^ ~})~%" characters modifiers)
      (if command
        (funcall command)
        (beep)))))


;;;; THE END ;;;;
