(require 'pjb-emacs)


(defun end-of-following-sexps ()
  "the position after the last of the sexps following the point until the end of buffer or end of list."
  (while (ignore-errors (progn (forward-sexp) t)))
  (point))


(defun fix-type-declaration ()
  "Transforms type declarations into check-type forms. Run with the cursor in front of the declare form."
  (interactive)
  (with-marker (end (progn (forward-sexp) (point)))
    (let ((start (progn
                   (backward-sexp)
                   (point)))
          (declare-form (sexp-at-point))
          (current-buffer (current-buffer)))
      (message "declare-form=%S" declare-form)
      (unless (and (listp declare-form)
                   (eq 'declare (first declare-form)))
        (error "Not a declare form at point."))
      (with-marker (end-of-body (end-of-following-sexps))
        (loop
           for decl in (rest declare-form)
           if (and (listp decl) (eq 'type (first decl)))
           collect decl into type-declarations
           else collect decl into other-declarations
           finally
             (when type-declarations
               (goto-char start)
               (delete-region start end)
               (when other-declarations
                 (pp `(declare ,@other-declarations) current-buffer))
               (dolist (decl (rest declare-form))
                 (when (eql 'type (first decl))
                   (dolist (var (cddr decl))
                     (pp `(check-type ,var ,(second decl)) current-buffer))))     
               (insert "(locally\n")
               (pp `(declare ,@type-declarations) current-buffer)
               (goto-char end-of-body)
               (insert ")")
               (indent-region start (point))))))))


 
