(ert-deftest output-white ()
  (let ((text "text"))
    (should (equal "\e[37mtext\e[0m" (ecukes-output-white text)))))

(ert-deftest output-red ()
  (let ((text "text"))
    (should (equal "\e[31mtext\e[0m" (ecukes-output-red text)))))

(ert-deftest output-green ()
  (let ((text "text"))
    (should (equal "\e[32mtext\e[0m" (ecukes-output-green text)))))
