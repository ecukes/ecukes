(ert-deftest run-hooks ()
  ;; Setup
  (setq setup-count 0)
  (Setup (setq setup-count (1+ setup-count)))
  
  ;; Teardown
  (setq teardown-count 0)
  (Teardown (setq teardown-count (1+ teardown-count)))
  
  ;; Before
  (setq before-count 0)
  (Before (setq before-count (1+ before-count)))
  
  ;; After
  (setq after-count 0)
  (After (setq after-count (1+ after-count)))
  
  (let ((feature (make-ecukes-feature :scenarios (list (mock-scenario) (mock-scenario)))))
    (quiet-message (ecukes-run-features (list feature))))
  
  (should (equal 1 setup-count))
  (should (equal 1 teardown-count))
  (should (equal 2 before-count))
  (should (equal 2 after-count)))

;; TODO: Add more tests...
