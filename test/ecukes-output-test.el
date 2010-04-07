(ert-deftest output-dont-save-in-history ()
  (ecukes-message-clear)
  (quiet-message
   (ecukes-output-message "some text..."))
  (should-not ecukes-message-history))

(ert-deftest output-newline ()
  (track-output
   (quiet-message
    (ecukes-output-newline))
   (should (equal " " (car message-output)))))

(ert-deftest output-white ()
  (track-output
   (quiet-message
    (ecukes-output-white "text"))
   (should (equal "\e[37mtext\e[0m" (car message-output)))))

(ert-deftest output-red ()
  (track-output
   (quiet-message
    (ecukes-output-red "text"))
   (should (equal "\e[31mtext\e[0m" (car message-output)))))

(ert-deftest output-green ()
  (track-output
   (quiet-message
    (ecukes-output-green "text"))
   (should (equal "\e[32mtext\e[0m" (car message-output)))))

(ert-deftest output-yellow ()
  (track-output
   (quiet-message
    (ecukes-output-yellow "text"))
   (should (equal "\e[33mtext\e[0m" (car message-output)))))

(ert-deftest output-text ()
  (track-output
   (quiet-message
    (ecukes-output-text "text"))
   (should (equal "text" (car message-output)))))

(ert-deftest output-text-with-offset ()
  (track-output
   (let ((ecukes-output-offset 2))
     (quiet-message
      (ecukes-output-text "text"))
     (should (equal "  text" (car message-output))))))

(ert-deftest output-text-with-offset ()
  (track-output
   (let ((ecukes-output-offset 2))
     (quiet-message
      (ecukes-output-no-indent "text"))
     (should (equal "text" (car message-output))))))

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
      "(Given \"this \\\"\\\\(.+\\\\)\\\"\"\n       (lambda (arg)\n\n         ))\n"))))

(ert-deftest output-missing-step-multiple-arguments ()
  (let ((step (make-ecukes-step :name "Given arguments \"argument1\" and \"argument2\"" :type 'regular)))
    (should
     (equal
      (ecukes-output-missing-step step)
      "(Given \"arguments \\\"\\\\(.+\\\\)\\\" and \\\"\\\\(.+\\\\)\\\"\"\n       (lambda (arg1 arg2)\n\n         ))\n"))))

(ert-deftest output-missing-step-non-regular ()
  (let ((step (make-ecukes-step :name "Given arguments \"argument1\" and \"argument2\"" :type 'table)))
    (should
     (equal
      (ecukes-output-missing-step step)
      "(Given \"arguments \\\"\\\\(.+\\\\)\\\" and \\\"\\\\(.+\\\\)\\\"\"\n       (lambda (arg1 arg2 arg3)\n\n         ))\n"))))

(ert-deftest output-intro ()
  (track-output
   (let ((ecukes-output-offset 0)
         (intro
          (make-ecukes-intro
           :header "Addition"
           :description '("In order to avoid silly mistakes"
                          "As a match idiot"
                          "I want to be told the sum of two numbers"))))
     (quiet-message (ecukes-output-intro intro))
     (should (equal (ecukes-color-white "Feature: Addition") (nth 0 message-output)))
     (should (equal (concat "  " (ecukes-color-white "In order to avoid silly mistakes")) (nth 1 message-output)))
     (should (equal (concat "  " (ecukes-color-white "As a match idiot")) (nth 2 message-output)))
     (should (equal (concat "  " (ecukes-color-white "I want to be told the sum of two numbers")) (nth 3 message-output)))
     (should (equal " " (nth 4 message-output)))
     (should (equal ecukes-output-offset 2)))))

(ert-deftest output-block ()
  (track-output
   (quiet-message
    (ecukes-output-block
     "Header:"
     (message "...")))
   (should (equal (ecukes-color-white "Header:") (nth 0 message-output)))
   (should (equal "..." (nth 1 message-output)))
   (should (equal " " (nth 2 message-output)))))

(ert-deftest output-scenario ()
  (track-output
   (quiet-message
    (let ((ecukes-output-offset 2)
          (scenario (make-ecukes-scenario :name "Do Something")))
      (ecukes-output-scenario scenario)
      ;; TODO: Print tags
      (should (equal (concat "  " (ecukes-color-white "Scenario: Do Something")) (nth 0 message-output)))
      (should (equal " " (nth 1 message-output)))))))

(ert-deftest output-background ()
  (track-output
   (quiet-message
    (let ((ecukes-output-offset 2)
          (background (make-ecukes-background)))
      (ecukes-output-background background)
      (should (equal (concat "  " (ecukes-color-white "Background:")) (nth 0 message-output)))
      (should (equal " " (nth 1 message-output)))))))

(ert-deftest output-regular-step ()
  (track-output
   (quiet-message
    (let ((step (make-ecukes-step :name "Given something" :type 'regular))
          (ecukes-output-offset 2))
      (ecukes-output-step step t)
      (should (equal (concat "  " (ecukes-color-green "Given something")) (nth 0 message-output)))))))

(ert-deftest output-py-string-step ()
  (track-output
   (quiet-message
    (let ((ecukes-output-offset 2)
          (step
           (make-ecukes-step
            :name "Given something"
            :type 'py-string
            :arg "Py String Arg")))
      (ecukes-output-step step t)
      (should (equal (concat "  " (ecukes-color-green "Given something")) (nth 0 message-output)))
      (should (equal (concat "    " (ecukes-color-green "\"\"\"")) (nth 1 message-output)))
      (should (equal (concat "    " (ecukes-color-green "Py String Arg")) (nth 2 message-output)))
      (should (equal (concat "    " (ecukes-color-green "\"\"\"")) (nth 3 message-output)))))))

(ert-deftest output-table-step ()
  (track-output
   (quiet-message
    (let ((ecukes-output-offset 2)
          (step
           (make-ecukes-step
            :name "Given something"
            :type 'table
            :arg
            (make-ecukes-table
             :header '("name" "age")
             :rows '(("Peter" "12") ("Clint" "37"))))))
      (ecukes-output-step step t)
      (should (equal (concat "  " (ecukes-color-green "Given something")) (nth 0 message-output)))
      (should (equal (concat "    " (ecukes-color-green "| name  | age | ")) (nth 1 message-output)))
      (should (equal (concat "    " (ecukes-color-green "| Peter | 12  | ")) (nth 2 message-output)))
      (should (equal (concat "    " (ecukes-color-green "| Clint | 37  | ")) (nth 3 message-output)))))))

(ert-deftest output-step-with-error ()
  (track-output
   (quiet-message
    (let ((step (make-ecukes-step :name "Given something" :type 'regular))
          (ecukes-output-offset 2))
      (ecukes-output-step step nil)
      (should (equal (concat "  " (ecukes-color-red "Given something")) (nth 0 message-output)))))))
