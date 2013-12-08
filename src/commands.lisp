(in-package :gsharp)

(define-command (com-rename-buffer :name t :command-table gsharp) ((new-name 'string :prompt "New name"))
  (rename-buffer (current-buffer) new-name :unique t))


(define-command (com-buffer-list :name t :command-table gsharp) ()
  (display-message "~{~A~%~}"  (mapcar (function buffer-name) (buffer-list))))

(set-key 'com-buffer-list 'gsharp '((#\x :control) (#\b :control)))
