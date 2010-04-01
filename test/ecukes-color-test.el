;; Color codes
(ert-deftest color-code-white ()
  (should (equal 37 ecukes-color-white)))

(ert-deftest color-code-red ()
  (should (equal 31 ecukes-color-red)))

(ert-deftest color-code-green ()
  (should (equal 32 ecukes-color-green)))

(ert-deftest color-code-yellow ()
  (should (equal 33 ecukes-color-yellow)))


;; Change color of single string
(ert-deftest color-single-text-white ()
  (should (equal "\e[37mtext\e[0m" (ecukes-color-white "text"))))

(ert-deftest color-single-text-red ()
  (should (equal "\e[31mtext\e[0m" (ecukes-color-red "text"))))

(ert-deftest color-single-text-green ()
  (should (equal "\e[32mtext\e[0m" (ecukes-color-green "text"))))

(ert-deftest color-single-text-yellow ()
  (should (equal "\e[33mtext\e[0m" (ecukes-color-yellow "text"))))


;; Change color of multiple strings
(ert-deftest color-multiple-texts-white ()
  (should (equal "\e[37mtext1 text2\e[0m" (ecukes-color-white "text1" " " "text2"))))

(ert-deftest color-multiple-texts-red ()
  (should (equal "\e[31mtext1 text2\e[0m" (ecukes-color-red "text1" " " "text2"))))

(ert-deftest color-multiple-texts-green ()
  (should (equal "\e[32mtext1 text2\e[0m" (ecukes-color-green "text1" " " "text2"))))

(ert-deftest color-multiple-texts-yellow ()
  (should (equal "\e[33mtext1 text2\e[0m" (ecukes-color-yellow "text1" " " "text2"))))
