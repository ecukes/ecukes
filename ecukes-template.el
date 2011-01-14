;;; ecukes-template.el --- Template helpers


(defvar ecukes-template-path
  (expand-file-name "templates" ecukes-path)
  "Path to templates directory.")


(defun ecukes-template-get (template &optional substitutions)
  "Return TEMPLATE with SUBSTITUTIONS as a string."
  (let ((template-file
         (expand-file-name (format "%s.tpl" (symbol-name template)) ecukes-template-path)))
    (if (file-exists-p template-file)
        (with-temp-buffer
          (insert-file-contents-literally template-file)
          (ecukes-template-substitute (buffer-string) substitutions))
      (error "Missing template file %s" template-file))))

(defun ecukes-template-substitute (string substitutions)
  "Substitute all SUBSTITUTIONS in STRING."
  (let ((result string))
    (mapc
     (lambda (substitution)
       (let ((old (car substitution))
             (new (cdr substitution)))
         (setq result (replace-regexp-in-string (format "{%s}" old) new result t))))
     substitutions)
    result))

(defun ecukes-template-write (write-to template &optional substitutions)
  "Write TEMPLATE to WRITE-TO with SUBSTITUTIONS."
  (let ((contents (ecukes-template-get template substitutions)))
    (with-temp-file write-to
      (insert contents))))


(provide 'ecukes-template)

;;; ecukes-template.el ends here
