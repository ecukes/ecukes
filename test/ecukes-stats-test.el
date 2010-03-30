(ert-deftest update-steps-success ()
    (reset-stats)
    (ecukes-stats-update-steps t)
    (should (equal 1 ecukes-stats-num-steps))
    (should (equal 0 ecukes-stats-num-steps-failed))
    (should (equal 1 ecukes-stats-num-steps-passed))
    (should (equal 0 ecukes-stats-num-scenarios))
    (should (equal 0 ecukes-stats-num-scenarios-failed))
    (should (equal 0 ecukes-stats-num-scenarios-passed)))

(ert-deftest update-steps-failure ()
    (reset-stats)
    (ecukes-stats-update-steps nil)
    (should (equal 1 ecukes-stats-num-steps))
    (should (equal 1 ecukes-stats-num-steps-failed))
    (should (equal 0 ecukes-stats-num-steps-passed))
    (should (equal 0 ecukes-stats-num-scenarios))
    (should (equal 0 ecukes-stats-num-scenarios-failed))
    (should (equal 0 ecukes-stats-num-scenarios-passed)))

(ert-deftest update-scenario-success ()
    (reset-stats)
    (ecukes-stats-update-scenarios t)
    (should (equal 0 ecukes-stats-num-steps))
    (should (equal 0 ecukes-stats-num-steps-failed))
    (should (equal 0 ecukes-stats-num-steps-passed))
    (should (equal 1 ecukes-stats-num-scenarios))
    (should (equal 0 ecukes-stats-num-scenarios-failed))
    (should (equal 1 ecukes-stats-num-scenarios-passed)))

(ert-deftest update-scenario-failure ()
    (reset-stats)
    (ecukes-stats-update-scenarios nil)
    (should (equal 0 ecukes-stats-num-steps))
    (should (equal 0 ecukes-stats-num-steps-failed))
    (should (equal 0 ecukes-stats-num-steps-passed))
    (should (equal 1 ecukes-stats-num-scenarios))
    (should (equal 1 ecukes-stats-num-scenarios-failed))
    (should (equal 0 ecukes-stats-num-scenarios-passed)))

(ert-deftest steps-summary ()
  (let ((ecukes-stats-num-steps 3)
        (ecukes-stats-num-steps-failed 2)
        (ecukes-stats-num-steps-passed 1))
    (should
     (equal
      (concat
       "\e["
       "37m"
       "3 steps ("
       "\e[0m"

       "\e["
       "31m"
       "2 failed"
       "\e[0m"

       "\e["
       "37m"
       ", "
       "\e[0m"

       "\e["
       "32m"
       "1 passed"
       "\e[0m"

       "\e["
       "37m"
       ")\n"
       "\e[0m"
       )
      (ecukes-stats-steps-summary)))))

(ert-deftest scenarios-summary ()
  (let ((ecukes-stats-num-scenarios 3)
        (ecukes-stats-num-scenarios-failed 2)
        (ecukes-stats-num-scenarios-passed 1))
    (should
     (equal
      (concat
       "\e["
       "37m"
       "3 scenarios ("
       "\e[0m"

       "\e["
       "31m"
       "2 failed"
       "\e[0m"

       "\e["
       "37m"
       ", "
       "\e[0m"

       "\e["
       "32m"
       "1 passed"
       "\e[0m"

       "\e["
       "37m"
       ")\n"
       "\e[0m"
       )
      (ecukes-stats-scenarios-summary)))))

(defun reset-stats ()
  (setq ecukes-stats-num-steps 0)
  (setq ecukes-stats-num-steps-failed 0)
  (setq ecukes-stats-num-steps-passed 0)
  (setq ecukes-stats-num-scenarios 0)
  (setq ecukes-stats-num-scenarios-failed 0)
  (setq ecukes-stats-num-scenarios-passed 0))
