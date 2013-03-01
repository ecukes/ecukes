(require 'ecukes-print)

(ert-deftest print-missing-steps ()
  "Should print header and steps."
  (with-mock
   (with-steps
    (Given "^state known$" 'ignore)
    (Given "^state unknown$" 'ignore)
    (let ((known (mock-step "Given state known"))
          (unknown (mock-step "And state unknown")))
      (mock (ecukes-print-message) :times 2)
      (mock (ecukes-print-missing-steps-header) :times 1)
      (ecukes-print-missing-steps (list known unknown))))))

(ert-deftest print-missing-steps-same-step ()
  "Should print header and same steps only once."
  (with-mock
   (with-steps
    (Given "^state known$" 'ignore)
    (Given "^state unknown$" 'ignore)
    (let ((known (mock-step "Given state known"))
          (unknown (mock-step "And state known")))
      (mock (ecukes-print-message) :times 1)
      (mock (ecukes-print-missing-steps-header) :times 1)
      (ecukes-print-missing-steps (list known unknown))))))

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
  (with-steps
   (Given "^a state$" 'ignore)
   (let ((step (mock-step "Given a state"))
         (expected
          (ansi-yellow "(Given \"^a state$\"\n       (lambda ()\n\n         ))")))
     (should (equal (ecukes-print-step-string step) expected)))))

(ert-deftest print-step-string-single-arg ()
  "Should print step when single arg."
  (with-steps
   (Given "^state \"\\(known\\)\"$" 'ignore)
   (let ((step (mock-step "Given state \"known\""))
         (expected
          (ansi-yellow "(Given \"^state \\\"\\\\([^\\\"]+\\\\)\\\"$\"\n       (lambda (arg)\n\n         ))")))
     (should (equal (ecukes-print-step-string step) expected)))))

(ert-deftest print-step-string-multiple-args ()
  "Should print step when multiple args."
  (with-steps
   (Given "^state \"\\(known\\)\" and \"\\(unknown\\)\"$" 'ignore)
   (let ((step (mock-step "Given state \"known\" and \"unknown\""))
         (expected
          (ansi-yellow "(Given \"^state \\\"\\\\([^\\\"]+\\\\)\\\" and \\\"\\\\([^\\\"]+\\\\)\\\"$\"\n       (lambda (arg-1 arg-2)\n\n         ))")))
     (should (equal (ecukes-print-step-string step) expected)))))

(ert-deftest print-step-args-no-args ()
  "Should be empty when no step args."
  (with-steps
   (Given "^a state$" 'ignore)
   (let ((step (mock-step "Given a state")))
     (should (equal (ecukes-print-step-args step) "")))))

(ert-deftest print-step-args-single-arg ()
  "Should return correct step args when single arg."
  (with-steps
   (Given "^state \"\\(known\\)\"$" 'ignore)
   (let ((step (mock-step "Given state \"known\"")))
     (should (equal (ecukes-print-step-args step) "arg")))))

(ert-deftest print-step-args-multiple-args ()
  "Should return correct step args when multiple args."
  (with-steps
   (Given "^state \"\\(known\\)\" and \"\\(unknown\\)\"$" 'ignore)
   (let ((step (mock-step "Given state \"known\" and \"unknown\"")))
     (should (equal (ecukes-print-step-args step) "arg-1 arg-2")))))

(ert-deftest print-step-args-with-args-and-table ()
  "Should return correct step args when args and table."
  (with-steps
   (Given "^state \"\\(known\\)\"$" 'ignore)
   (let ((step (mock-step "Given state \"known\"" :type 'table)))
     (should (equal (ecukes-print-step-args step) "arg-1 arg-2")))))

(ert-deftest print-step-args-py-string ()
  "Should return correct step args when args and py-string."
  (with-steps
   (Given "^a known state$" 'ignore)
   (let ((step (mock-step "Given a known state" :type 'py-string)))
     (should (equal (ecukes-print-step-args step) "arg")))))

(ert-deftest print-step-args-table ()
  "Should return correct step args when args and table."
  (with-steps
   (Given "^a known state$" 'ignore)
   (let ((step (mock-step "Given a known state" :type 'table)))
     (should (equal (ecukes-print-step-args step) "arg")))))

(ert-deftest print-step-args-with-args-and-py-string ()
  "Should return correct step args when args and py-string."
  (with-steps
   (Given "^state \"\\(known\\)\"$" 'ignore)
   (let ((step (mock-step "Given state \"known\"" :type 'py-string)))
     (should (equal (ecukes-print-step-args step) "arg-1 arg-2")))))

(ert-deftest print-step-body-no-args ()
  "Should be empty when no step args."
  (with-steps
   (Given "^a state$" 'ignore)
   (let ((step (mock-step "Given a state")))
     (should (equal (ecukes-print-step-body step) "a state")))))

(ert-deftest print-step-body-single-arg ()
  "Should return correct step body when single arg."
  (with-steps
   (Given "^state \"\\(known\\)\"$" 'ignore)
   (let ((step (mock-step "Given state \"known\""))
         (expected "state \\\"\\\\([^\\\"]+\\\\)\\\""))
     (should (equal (ecukes-print-step-body step) expected)))))

(ert-deftest print-step-body-multiple-args ()
  "Should return correct step body when multiple args."
  (with-steps
   (Given "^state \"\\(known\\)\" and \"\\(unknown\\)\"$" 'ignore)
   (let ((step (mock-step "Given state \"known\" and \"unknown\""))
         (expected "state \\\"\\\\([^\\\"]+\\\\)\\\" and \\\"\\\\([^\\\"]+\\\\)\\\""))
     (should (equal (ecukes-print-step-body step) expected)))))

(ert-deftest print-intro ()
  "Should print intro."
  (with-messages
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
   (with-messages
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
  "Should print formatted message."
  (with-mock
   (mock (ecukes-print-format "MESSAGE" nil) :times 1)
   (with-messages
    (lambda (messages)
      (ecukes-print-message "MESSAGE")))))

(ert-deftest format-message-simple ()
  "Should format message when simple."
  (should (equal (ecukes-print-format "MESSAGE") "MESSAGE")))

(ert-deftest format-message ()
  "Should format message."
  (should (equal (ecukes-print-format "MESSAGE %s" "EGASSEM") "MESSAGE EGASSEM")))

(ert-deftest format-message-offset ()
  "Should format message with offset."
  (let ((ecukes-print-offset 2))
    (should (equal (ecukes-print-format "MESSAGE") "  MESSAGE"))))

(ert-deftest print-background-header ()
  "Should print background header."
  (with-messages
   (lambda (messages)
     (ecukes-print-background-header)
     (should (equal messages (list "  Background:"))))))

(ert-deftest print-scenario-header ()
  "Should print scenario header."
  (with-messages
   (lambda (messages)
     (let ((scenario (make-ecukes-scenario :name "Simple")))
       (ecukes-print-scenario-header scenario)
       (should (equal messages (list "  Scenario: Simple")))))))

(ert-deftest print-scenario-header-tags ()
  "Should print scenario header when tags."
  (with-messages
   (lambda (messages)
     (let ((scenario
            (make-ecukes-scenario :name "Simple" :tags (list "foo" "bar")))
           (expected
            (list (format "  %s" (ansi-cyan "@foo @bar")) "  Scenario: Simple")))
       (ecukes-print-scenario-header scenario)
       (should (equal messages expected))))))

(ert-deftest print-step-success ()
  "Should print successful step."
  (with-messages
   (lambda (messages)
     (let ((step (mock-step "Given a state"))
           (expected (list (format "    %s" (ansi-green "Given a state")))))
       (ecukes-print-step step 'success)
       (should (equal messages expected))))))

(ert-deftest print-step-failure ()
  "Should print successful step."
  (with-messages
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
  (with-messages
   (lambda (messages)
     (let ((step (mock-step "Given a state"))
           (expected
            (list
             (format "    %s" (ansi-cyan "Given a state")))))
       (ecukes-print-step step 'skipped)
       (should (equal messages expected))))))

(ert-deftest print-step-failure-no-error ()
  "Should print failed step no error."
  (with-messages
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
  (with-messages
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
   (with-messages
    (lambda (messages)
      (let ((step
             (mock-step "Given this:" :type 'table)))
        (ecukes-print-step step 'success))))))

(ert-deftest print-step-py-string ()
  "Should print py-string if step is py-string step."
  (with-mock
   (mock (ecukes-print-py-string) :times 1)
   (with-messages
    (lambda (messages)
      (let ((step
             (mock-step "Given this:" :type 'py-string)))
        (ecukes-print-step step 'success))))))

(ert-deftest print-step-table-success ()
  "Should print table if step is table step when success."
  (with-messages
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
  (with-messages
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
  (with-messages
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
  (with-messages
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
  (with-messages
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
  (with-messages
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

(ert-deftest print-steps-no-docs-or-file ()
  "Should print list of steps with no docs or file."
  (with-steps
   (Given "^a known state$" 'ignore)
   (Given "^an unknown state$" 'ignore)
   (with-messages
    (lambda (messages)
      (let ((expected
             (list
              (ansi-green "^an unknown state$")
              (ansi-green "^a known state$"))))
        (ecukes-print-steps)
        (should (equal messages expected)))))))

(ert-deftest print-steps-with-docs ()
  "Should print list of steps with docs but no file."
  (with-steps
   (Given "^a known state$" "Will be in a known state" 'ignore)
   (Given "^an unknown state$" "Will be in an unknown state" 'ignore)
   (with-messages
    (lambda (messages)
      (let ((expected
             (list
              (concat (ansi-green "^an unknown state$") "\n" (ansi-cyan "Will be in an unknown state") "\n")
              (concat (ansi-green "^a known state$") "\n" (ansi-cyan "Will be in a known state") "\n"))))
        (ecukes-print-steps t)
        (should (equal messages expected)))))))

(ert-deftest print-steps-with-file ()
  "Should print list of steps with file but no docs."
  (with-steps
   (Given "^a known state$" "Will be in a known state" 'ignore)
   (Given "^an unknown state$" "Will be in an unknown state" 'ignore)
   (with-messages
    (lambda (messages)
      (let ((expected
             (list
              (concat "test/ecukes-test: " (ansi-green "^an unknown state$"))
              (concat "test/ecukes-test: " (ansi-green "^a known state$")))))
      (ecukes-print-steps nil t)
      (should (equal messages expected)))))))

(ert-deftest print-steps-with-file-and-docs ()
  "Should print list of steps with file and docs."
  (with-steps
   (Given "^a known state$" "Will be in a known state" 'ignore)
   (Given "^an unknown state$" "Will be in an unknown state" 'ignore)
   (with-messages
    (lambda (messages)
      (let ((expected
             (list
              (concat "test/ecukes-test: " (ansi-green "^an unknown state$") "\n" (ansi-cyan "Will be in an unknown state") "\n")
              (concat "test/ecukes-test: " (ansi-green "^a known state$") "\n" (ansi-cyan "Will be in a known state") "\n"))))
      (ecukes-print-steps t t)
      (should (equal messages expected)))))))

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
