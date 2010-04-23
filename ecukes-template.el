;;; ecukes-template.el --- Template stuff


(defvar ecukes-template-path (expand-file-name "templates" ecukes-path)
  "Path to directory containing templates.")


(defun ecukes-template-get (template &optional replacements)
  "Returns TEMPLATE with REPLACEMENTS as a string.

REPLACEMENTS is an association list where the key is a string to
replace with the value. Keys in the template file should be wrapped
with curly braces.

For example:
  (ecukes-template-get \"name\" `((\"key1\" . ,var1) (\"key2\" . \"value2\")))
will replace all occurrences of {key1} with the value in the variable
var1 and {key2} with the string value2."
  (let ((template-file (ecukes-template-file template)))
    (with-temp-buffer
      (insert-file-contents-literally template-file)
      (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
        (ecukes-template-assign buffer-contents replacements)))))

(defun ecukes-template-assign (contents replacements)
  "Return CONTENTS with REPLACEMENTS."
  (dolist (replacement replacements)
    (setq contents (ecukes-template-replace contents (car replacement) (cdr replacement))))
  contents)

(defun ecukes-template-replace (contents from to)
  "Replaces all occurrences of FROM in contents wrapped in curly braces with TO."
  (replace-regexp-in-string (concat "{" from "}") to contents t))

(defun ecukes-template-file (name)
  "Returns full path to template with NAME."
  (expand-file-name (concat name ".tpl") ecukes-template-path))


(provide 'ecukes-template)

;;; ecukes-template.el ends here
