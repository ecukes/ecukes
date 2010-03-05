(ert-deftest ecukes-output-white ()
  (let ((text "text"))
    (should (equal "\e[37mtext\e[0m" (ecukes-output-white text)))))

(ert-deftest ecukes-output-white-with-offset ()
  (let ((text "text") (ecukes-output-offset 2))
     (should (equal "\e[37m  text\e[0m" (ecukes-output-white text)))))
