(ert-deftest run-hooks ()
  ;; Before
  (setq before-count 0)
  (Before (setq before-count (1+ before-count)))
  
  ;; After
  (setq after-count 0)
  (After (setq after-count (1+ after-count)))
  
  (let ((feature (make-ecukes-feature :scenarios (list (mock-scenario) (mock-scenario)))))
    (quiet-message (ecukes-run-features (list feature))))
  
  (should (equal 2 before-count))
  (should (equal 2 after-count)))

(ert-deftest run-clear-message-history ()
  (quiet-message
   (let ((mess "mess..."))
     (message mess)
     (let ((feature (make-ecukes-feature :scenarios (list (mock-scenario) (mock-scenario)))))
       (ecukes-run-features (list feature)))
     (should-not (member mess ecukes-message-history)))))

;; TODO: Add more tests...
