;;; ecukes-color.el --- Colors for printing

(defconst ecukes-color-white 37
  "White color code.")

(defconst ecukes-color-red 31
  "Red color code.")

(defconst ecukes-color-green 32
  "Green color code.")


(defmacro ecukes-color-white (&rest body)
  "Returns BODY as a concatenated string in white."
  `(ecukes-color-apply ecukes-color-white ,@body))

(defmacro ecukes-color-green (&rest body)
  "Returns BODY as a concatenated string in green."
  `(ecukes-color-apply ecukes-color-green ,@body))

(defmacro ecukes-color-red (&rest body)
  "Returns BODY as a concatenated string in red."
  `(ecukes-color-apply ecukes-color-red ,@body))

(defun ecukes-color-apply (color &rest body)
  "Returns BODY as a concatenated string in COLOR."
  (let ((text (apply 'concat body)))
    (concat "\e[" (number-to-string color) "m" text "\e[0m")))


(provide 'ecukes-color)

;;; ecukes-color.el ends here
