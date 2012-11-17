(require 'ecukes-print)

(ert-deftest print-missing-steps ()
  "Should print header and steps."
  (with-mock
   (let ((known (mock-step "Given state known"))
         (unknown (mock-step "And state unknown")))
     (mock (ecukes-print-message) :times 2)
     (mock (ecukes-print-missing-steps-header) :times 1)
     (ecukes-print-missing-steps (list known unknown)))))

(ert-deftest print-missing-steps-same-step ()
  "Should print header and same steps only once."
  (with-mock
   (let ((known (mock-step "Given state known"))
         (unknown (mock-step "And state known")))
     (mock (ecukes-print-message) :times 1)
     (mock (ecukes-print-missing-steps-header) :times 1)
     (ecukes-print-missing-steps (list known unknown)))))

(ert-deftest print-missing-steps-header ()
  "Should print header."
  (with-mock
   (mock
    (ecukes-print-message
     (ansi-yellow
      "Some steps does not have a matching definition. Please implement the following step definitions:\n"))
    :times 1)
   (ecukes-print-missing-steps-header)))

(ert-deftest print-step-string-no-args ()
  "Should print step when no args."
  (let ((step (mock-step "Given a state"))
        (expected "(Given \"^a state$\"\n       (lambda ()\n\n         ))"))
    (should (equal (ecukes-print-step-string step) expected))))

(ert-deftest print-step-string-single-arg ()
  "Should print step when single arg."
  (let ((step (mock-step "Given state \"known\""))
        (expected "(Given \"^state \\\"\\\\([^\\\"]+\\\\)\\\"$\"\n       (lambda (arg)\n\n         ))"))
    (should (equal (ecukes-print-step-string step) expected))))

(ert-deftest print-step-string-multiple-args ()
  "Should print step when multiple args."
  (let ((step (mock-step "Given state \"known\" and \"unknown\""))
        (expected "(Given \"^state \\\"\\\\([^\\\"]+\\\\)\\\" and \\\"\\\\([^\\\"]+\\\\)\\\"$\"\n       (lambda (arg-1 arg-2)\n\n         ))"))
    (should (equal (ecukes-print-step-string step) expected))))

(ert-deftest print-step-args-no-args ()
  "Should be empty when no step args."
  (let ((step (mock-step "Given a state")))
    (should (equal (ecukes-print-step-args step) ""))))

(ert-deftest print-step-args-single-arg ()
  "Should return correct step args when single arg."
  (let ((step (mock-step "Given state \"known\"")))
    (should (equal (ecukes-print-step-args step) "arg"))))

(ert-deftest print-step-args-multiple-args ()
  "Should return correct step args when multiple args."
  (let ((step (mock-step "Given state \"known\" and \"unknown\"")))
    (should (equal (ecukes-print-step-args step) "arg-1 arg-2"))))

(ert-deftest print-step-args-with-args-and-table ()
  "Should return correct step args when args and table."
  (let ((step (mock-step "Given state \"known\"" :type 'table)))
    (should (equal (ecukes-print-step-args step) "arg-1 arg-2"))))

(ert-deftest print-step-args-py-string ()
  "Should return correct step args when args and py-string."
  (let ((step (mock-step "Given a known state" :type 'py-string)))
    (should (equal (ecukes-print-step-args step) "arg"))))

(ert-deftest print-step-args-table ()
  "Should return correct step args when args and table."
  (let ((step (mock-step "Given a known state" :type 'table)))
    (should (equal (ecukes-print-step-args step) "arg"))))

(ert-deftest print-step-args-with-args-and-py-string ()
  "Should return correct step args when args and py-string."
  (let ((step (mock-step "Given state \"known\"" :type 'py-string)))
    (should (equal (ecukes-print-step-args step) "arg-1 arg-2"))))

(ert-deftest print-step-body-no-args ()
  "Should be empty when no step args."
  (let ((step (mock-step "Given a state")))
    (should (equal (ecukes-print-step-body step) "a state"))))

(ert-deftest print-step-body-single-arg ()
  "Should return correct step body when single arg."
  (let ((step (mock-step "Given state \"known\""))
        (expected "state \\\"\\\\([^\\\"]+\\\\)\\\""))
    (should (equal (ecukes-print-step-body step) expected))))

(ert-deftest print-step-body-multiple-args ()
  "Should return correct step body when multiple args."
  (let ((step (mock-step "Given state \"known\" and \"unknown\""))
        (expected "state \\\"\\\\([^\\\"]+\\\\)\\\" and \\\"\\\\([^\\\"]+\\\\)\\\""))
    (should (equal (ecukes-print-step-body step) expected))))

(ert-deftest print-intro ()
  "Should print intro."
  (with-message
   (lambda (messages)
     (with-parse-feature
      "simple"
      (lambda (feature intro scenarios background steps)
        (ecukes-print-intro intro)
        (let ((expected (list "Feature: Simple" "  This" "  Is" "  Simple" " ")))
          (should (equal messages expected))))))))

(ert-deftest print-stats-summary ()
  "Should print stats summary."
  (with-mock
   (stub ecukes-stats-summary => "SUMMARY")
   (with-message
    (lambda (messages)
      (ecukes-print-stats-summary)
      (should
       (equal messages (list "SUMMARY")))))))

(ert-deftest print-newline ()
  "Should print newline."
  (with-mock
   (mock (ecukes-print-message " ") :times 1)
   (ecukes-print-newline)))

(ert-deftest print-message ()
  "Should print message."
  (with-message
   (lambda (messages)
     (let ((message "MESSAGE"))
       (ecukes-print-message message)
       (should (equal messages (list message)))))))

(ert-deftest print-message-offset ()
  "Should print message with offset."
  (with-message
   (lambda (messages)
     (ecukes-print-message "MESSAGE")
     (let ((ecukes-print-offset 2))
       (ecukes-print-message "MESSAGE"))
     (should (equal messages (list "MESSAGE" "  MESSAGE"))))))

(ert-deftest print-background-header ()
  "Should print background header."
  (with-message
   (lambda (messages)
     (ecukes-print-background-header)
     (should (equal messages (list "  Background:"))))))

(ert-deftest print-scenario-header ()
  "Should print scenario header."
  (with-message
   (lambda (messages)
     (let ((scenario (make-ecukes-scenario :name "Simple")))
       (ecukes-print-scenario-header scenario)
       (should (equal messages (list "  Scenario: Simple")))))))

(ert-deftest print-step-success ()
  "Should print successful step."
  (with-message
   (lambda (messages)
     (let ((step (mock-step "Given a state"))
           (expected (list (format "    %s" (ansi-green "Given a state")))))
       (ecukes-print-step step 'success)
       (should (equal messages expected))))))

(ert-deftest print-step-failure ()
  "Should print successful step."
  (with-message
   (lambda (messages)
     (let ((step (mock-step "Given a state" :err "ERROR"))
           (expected
            (list
             (format "    %s" (ansi-red "Given a state"))
             (format "      %s" (ansi-red "ERROR")))))
       (ecukes-print-step step 'failure)
       (should (equal messages expected))))))

(ert-deftest print-step-skipped ()
  "Should print skipped step."
  (with-message
   (lambda (messages)
     (let ((step (mock-step "Given a state"))
           (expected
            (list
             (format "    %s" (ansi-cyan "Given a state")))))
       (ecukes-print-step step 'skipped)
       (should (equal messages expected))))))

(ert-deftest print-step-failure-no-error ()
  "Should print failed step no error."
  (with-message
   (lambda (messages)
     (let ((step (mock-step "Given a state"))
           (expected
            (list
             (format "    %s" (ansi-red "Given a state"))
             (format "      %s" (ansi-red "Unknown error...")))))
       (ecukes-print-step step 'failure)
       (should (equal messages expected))))))

(ert-deftest print-step-failure-multi-line-error ()
  "Should print successful failure multi line error."
  (with-message
   (lambda (messages)
     (let ((step (mock-step "Given a state" :err "KNOWN\nERROR"))
           (expected
            (list
             (format "    %s" (ansi-red "Given a state"))
             (format "      %s" (ansi-red "KNOWN"))
             (format "      %s" (ansi-red "ERROR")))))
       (ecukes-print-step step 'failure)
       (should (equal messages expected))))))

(ert-deftest print-step-table ()
  "Should print table if step is table step."
  (with-mock
   (mock (ecukes-print-table) :times 1)
   (with-message
    (lambda (messages)
      (let ((step
             (mock-step "Given this:" :type 'table)))
        (ecukes-print-step step 'success))))))

(ert-deftest print-step-py-string ()
  "Should print py-string if step is py-string step."
  (with-mock
   (mock (ecukes-print-py-string) :times 1)
   (with-message
    (lambda (messages)
      (let ((step
             (mock-step "Given this:" :type 'py-string)))
        (ecukes-print-step step 'success))))))

(ert-deftest print-step-table-success ()
  "Should print table if step is table step when success."
  (with-message
   (lambda (messages)
     (let ((step (mock-step "Given this:" :type 'table :arg '(("x" "y") ("x1" "y1") ("x2" "y2"))))
           (expected
            (list
             (format "      | %s  | %s  |" (ansi-green "x") (ansi-green "y"))
             (format "      | %s | %s |" (ansi-green "x1") (ansi-green "y1"))
             (format "      | %s | %s |" (ansi-green "x2") (ansi-green "y2")))))
       (ecukes-print-table step 'success)
       (should (equal messages expected))))))

(ert-deftest print-step-table-failure ()
  "Should print table if step is table step when failure."
  (with-message
   (lambda (messages)
     (let ((step (mock-step "Given this:" :type 'table :arg '(("x" "y") ("x1" "y1") ("x2" "y2"))))
           (expected
            (list
             (format "      | %s  | %s  |" (ansi-red "x") (ansi-red "y"))
             (format "      | %s | %s |" (ansi-red "x1") (ansi-red "y1"))
             (format "      | %s | %s |" (ansi-red "x2") (ansi-red "y2")))))
       (ecukes-print-table step 'failure)
       (should (equal messages expected))))))

(ert-deftest print-step-table-skipped ()
  "Should print table if step is table step when skipped."
  (with-message
   (lambda (messages)
     (let ((step (mock-step "Given this:" :type 'table :arg '(("x" "y") ("x1" "y1") ("x2" "y2"))))
           (expected
            (list
             (format "      | %s  | %s  |" (ansi-cyan "x") (ansi-cyan "y"))
             (format "      | %s | %s |" (ansi-cyan "x1") (ansi-cyan "y1"))
             (format "      | %s | %s |" (ansi-cyan "x2") (ansi-cyan "y2")))))
       (ecukes-print-table step 'skipped)
       (should (equal messages expected))))))

(ert-deftest print-step-py-string-success ()
  "Should print py-string if step is py-string step when success."
  (with-message
   (lambda (messages)
     (let ((step (mock-step "Given this:" :type 'table :arg "foo\nbar\nbaz\nqux"))
           (expected
            (list
             (format "      %s" (ansi-green "\"\"\""))
             (format "      %s" (ansi-green "foo"))
             (format "      %s" (ansi-green "bar"))
             (format "      %s" (ansi-green "baz"))
             (format "      %s" (ansi-green "qux"))
             (format "      %s" (ansi-green "\"\"\"")))))
       (ecukes-print-py-string step 'success)
       (should (equal messages expected))))))

(ert-deftest print-step-py-string-failure ()
  "Should print py-string if step is py-string step when failure."
  (with-message
   (lambda (messages)
     (let ((step (mock-step "Given this:" :type 'table :arg "foo\nbar\nbaz\nqux"))
           (expected
            (list
             (format "      %s" (ansi-red "\"\"\""))
             (format "      %s" (ansi-red "foo"))
             (format "      %s" (ansi-red "bar"))
             (format "      %s" (ansi-red "baz"))
             (format "      %s" (ansi-red "qux"))
             (format "      %s" (ansi-red "\"\"\"")))))
       (ecukes-print-py-string step 'failure)
       (should (equal messages expected))))))

(ert-deftest print-step-py-string-skipped ()
  "Should print py-string if step is py-string step when skipped."
  (with-message
   (lambda (messages)
     (let ((step (mock-step "Given this:" :type 'table :arg "foo\nbar\nbaz\nqux"))
           (expected
            (list
             (format "      %s" (ansi-cyan "\"\"\""))
             (format "      %s" (ansi-cyan "foo"))
             (format "      %s" (ansi-cyan "bar"))
             (format "      %s" (ansi-cyan "baz"))
             (format "      %s" (ansi-cyan "qux"))
             (format "      %s" (ansi-cyan "\"\"\"")))))
       (ecukes-print-py-string step 'skipped)
       (should (equal messages expected))))))

(ert-deftest print-status-success ()
  "Should print in green when success."
  (with-mock
   (mock (ansi-green "SUCCESS") :times 1)
   (ecukes-print-status "SUCCESS" 'success)))

(ert-deftest print-status-failure ()
  "Should print in green when failure."
  (with-mock
   (mock (ansi-red "FAILURE") :times 1)
   (ecukes-print-status "FAILURE" 'failure)))

(ert-deftest print-status-skipped ()
  "Should print in green when skipped."
  (with-mock
   (mock (ansi-cyan "SKIPPED") :times 1)
   (ecukes-print-status "SKIPPED" 'skipped)))
