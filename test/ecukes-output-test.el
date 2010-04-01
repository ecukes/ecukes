(ert-deftest output-intro ()
  (let ((ecukes-output-offset 0))
    (ecukes-output-intro (mock-intro))
    (should (equal ecukes-output-offset 2))))

(ert-deftest output-newline ()
  (should (equal " \n" (ecukes-output-newline))))

(ert-deftest output-white ()
  (should (equal "\e[37mtext\e[0m\n" (ecukes-output-white "text"))))

(ert-deftest output-red ()
  (should (equal "\e[31mtext\e[0m\n" (ecukes-output-red "text"))))

(ert-deftest output-green ()
  (should (equal "\e[32mtext\e[0m\n" (ecukes-output-green "text"))))

(ert-deftest output-text ()
  (should (equal "text\n" (ecukes-output-text "text"))))

(ert-deftest output-text-with-offset ()
  (let ((ecukes-output-offset 2))
    (should (equal "  text\n" (ecukes-output-text "text")))))

(ert-deftest output-no-indent ()
  (let ((ecukes-output-offset 2))
    (should (equal "text\n" (ecukes-output-no-indent "text")))))

(ert-deftest output-table-row ()
  (let ((cols '("first" "second" "third"))
        (widths '(5 6 5))
        (expected "| first | second | third | "))
    (should (equal expected (ecukes-output-table-row cols widths)))))

(ert-deftest output-missing-step-simple ()
  (let ((step (make-ecukes-step :name "Given something" :type 'regular)))
    (should
     (equal
      (ecukes-output-missing-step step)
      "(Given \"something\"\n       (lambda ()\n\n         ))\n"))))

(ert-deftest output-missing-step-single-argument ()
  (let ((step (make-ecukes-step :name "Given this \"argument\"" :type 'regular)))
    (should
     (equal
      (ecukes-output-missing-step step)
      "(Given \"this \\\"\\\\(.+\\\\)\\\"\"\n       (lambda ()\n\n         ))\n"))))

(ert-deftest output-missing-step-multiple-arguments ()
  (let ((step (make-ecukes-step :name "Given arguments \"argument1\" and \"argument2\"" :type 'regular)))
    (should
     (equal
      (ecukes-output-missing-step step)
      "(Given \"arguments \\\"\\\\(.+\\\\)\\\" and \\\"\\\\(.+\\\\)\\\"\"\n       (lambda ()\n\n         ))\n"))))
