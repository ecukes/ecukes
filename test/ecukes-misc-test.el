(ert-deftest clear-messages ()
  (let ((mess "mess..."))
    (quiet-message
     (message mess))
    (ecukes-clear-messages)
    (should-not
     (search
      mess
      (save-excursion
        (set-buffer "*Messages*")
        (buffer-substring-no-properties (point-min) (point-max)))))))
