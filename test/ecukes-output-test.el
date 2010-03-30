(ert-deftest output-intro ()
  (let ((ecukes-output-offset 0))
    (ecukes-output-intro (mock-intro))
    (should (equal ecukes-output-offset 2))))

(ert-deftest output-newline ()
  (should (equal " " (ecukes-output-newline))))

(ert-deftest output-white ()
  (should (equal "\e[37mtext\e[0m" (ecukes-output-white "text"))))

(ert-deftest output-red ()
  (should (equal "\e[31mtext\e[0m" (ecukes-output-red "text"))))

(ert-deftest output-green ()
  (should (equal "\e[32mtext\e[0m" (ecukes-output-green "text"))))

(ert-deftest output-text ()
  (should (equal "text" (ecukes-output-text "text"))))

(ert-deftest output-text-with-offset ()
  (let ((ecukes-output-offset 2))
    (should (equal "  text" (ecukes-output-text "text")))))

(ert-deftest output-no-indent ()
  (let ((ecukes-output-offset 2))
    (should (equal "text" (ecukes-output-no-indent "text")))))

(ert-deftest output-table-row ()
  (let ((cols '("first" "second" "third"))
        (widths '(5 6 5))
        (expected "| first | second | third | "))
    (should (equal expected (ecukes-output-table-row cols widths)))))
