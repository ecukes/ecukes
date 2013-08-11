;;; ecukes-template.el --- Template helpers

(require 'dash)
(require 's)

(defvar ecukes-path (f-dirname load-file-name)
  "Path to ecukes.")

(defvar ecukes-template-path
  (f-expand "templates" ecukes-path)
  "Path to templates directory.")


(defun ecukes-template-get (template &optional substitutions)
  "Return TEMPLATE with SUBSTITUTIONS as a string."
  (let ((template-file
         (f-expand (format "%s.tpl" (symbol-name template)) ecukes-template-path)))
    (if (f-file? template-file)
        (ecukes-template-substitute (f-read template-file) substitutions)
      (error "Missing template file %s" template-file))))

(defun ecukes-template-substitute (string substitutions)
  "Substitute all SUBSTITUTIONS in STRING."
  (-each
   substitutions
   (lambda (substitution)
     (let ((old (car substitution))
           (new (cdr substitution)))
       (setq string (s-replace (format "{{%s}}" old) new string)))))
  string)

(defun ecukes-template-write (write-to template &optional substitutions)
  "Write TEMPLATE to WRITE-TO with SUBSTITUTIONS."
  (let ((contents (ecukes-template-get template substitutions)))
    (f-write write-to contents)))


(provide 'ecukes-template)

;;; ecukes-template.el ends here
