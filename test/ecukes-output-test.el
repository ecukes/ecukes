(ert-deftest output-white ()
  (let ((text "text"))
    (should (equal "\e[37mtext\e[0m" (ecukes-output-white text)))))
