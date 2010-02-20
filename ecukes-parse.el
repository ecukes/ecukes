;;; ecukes-parse.el --- Parser stuff

(defstruct ecukes-feature intro)
(defstruct ecukes-intro header description)

(defvar ecukes-feature-re "Feature: *\\(.+[^ ]\\) *$"
  "Regular expression matching a feature header.")

(defun ecukes-parse-feature (feature-file)
  (with-temp-buffer
    (insert-file-contents-literally feature-file)
    (goto-char (point-min))

    (let ((intro (ecukes-parse-intro)))
      (make-ecukes-feature :intro intro)
      )

    )
  )

(defun ecukes-parse-intro ()
  "Parses the intro of a feature.

Example

  Feature: Addition of two numbers
    In order to aviod silly mistakes
    As a math idiot
    I want to be told the sum of two numbers
"
  (when (re-search-forward ecukes-feature-re nil t)
    (let ((header (match-string-no-properties 1))
          (description (list)))
      (forward-line 1)
      (while (not (string= (ecukes-line) ""))
        (add-to-list 'description (ecukes-line) t)
        (forward-line 1))
      (make-ecukes-intro :header header :description description))))

(defun ecukes-line (&optional n)
  "Returns the current line +-N at point with removed leading and
trailing whitespace."
  (forward-line (or n 0))
  (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (string-match "^ *\\(.*\\) *$" line)
    (match-string-no-properties 1 line)))

(provide 'ecukes-parse)

;;; ecukes-parse.el ends here
