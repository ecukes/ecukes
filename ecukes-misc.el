;;; ecukes-misc.el --- Miscellaneous helper functions


(defun ecukes-clear-messages ()
  "Clears all text in *Messages* buffer."
  (save-excursion
    (set-buffer "*Messages*")
    (erase-buffer)))


(provide 'ecukes-misc)

;;; ecukes-misc.el ends here
