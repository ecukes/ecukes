(require 'ecukes-stats)

(ert-deftest stats-reset ()
  "Should reset stats."
  (with-stats
   (setq ecukes-stats-steps 1)
   (setq ecukes-stats-steps-passed 2)
   (setq ecukes-stats-steps-failed 3)
   (setq ecukes-stats-steps-skipped 4)
   (setq ecukes-stats-scenarios 5)
   (setq ecukes-stats-scenarios-passed 6)
   (setq ecukes-stats-scenarios-failed 7)

   (ecukes-stats-reset)

   (should (equal ecukes-stats-steps 0))
   (should (equal ecukes-stats-steps-passed 0))
   (should (equal ecukes-stats-steps-failed 0))
   (should (equal ecukes-stats-steps-skipped 0))
   (should (equal ecukes-stats-scenarios 0))
   (should (equal ecukes-stats-scenarios-passed 0))
   (should (equal ecukes-stats-scenarios-failed 0))))

(ert-deftest stats-update-num-steps ()
  "Should update number of steps."
  (with-stats
   (ecukes-stats-step-pass)
   (ecukes-stats-step-fail)
   (ecukes-stats-step-skip)
   (should (equal ecukes-stats-steps 3))))

(ert-deftest stats-update-passed-steps ()
  "Should update number of passed steps."
  (with-stats
   (ecukes-stats-step-pass)
   (should (equal ecukes-stats-steps-passed 1))
   (should (equal ecukes-stats-steps 1))))

(ert-deftest stats-update-failed-steps ()
  "Should update number of failed steps."
  (with-stats
   (ecukes-stats-step-fail)
   (should (equal ecukes-stats-steps-failed 1))
   (should (equal ecukes-stats-steps 1))))

(ert-deftest stats-update-skipped-steps ()
  "Should update number of skipped steps."
  (with-stats
   (ecukes-stats-step-skip)
   (should (equal ecukes-stats-steps-skipped 1))
   (should (equal ecukes-stats-steps 1))))

(ert-deftest stats-update-num-scenarios ()
  "Should update number of scenarios."
  (with-stats
   (ecukes-stats-scenario-pass)
   (ecukes-stats-scenario-fail)
   (should (equal ecukes-stats-scenarios 2))))

(ert-deftest stats-update-passed-scenarios ()
  "Should update number of passed scenarios."
  (with-stats
   (ecukes-stats-scenario-pass)
   (should (equal ecukes-stats-scenarios-passed 1))
   (should (equal ecukes-stats-scenarios 1))))

(ert-deftest stats-update-failed-scenarios ()
  "Should update number of failed scenarios."
  (with-stats
   (ecukes-stats-scenario-fail)
   (should (equal ecukes-stats-scenarios-failed 1))
   (should (equal ecukes-stats-scenarios 1))))
