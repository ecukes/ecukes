;; TODO: Mock message to avoid output

(ert-deftest output-intro ()
  (let ((ecukes-output-offset 0))
    (ecukes-output-intro (mock-intro))
    (should (equal ecukes-output-offset 2))))

(ert-deftest output-scenario ()
  (let ((ecukes-output-offset 0))
    (should (equal "Addition:" (ecukes-output-scenario (mock-scenario))))
    (should (equal ecukes-output-offset 2))))

(ert-deftest output-background ()
  (let ((ecukes-output-offset 0))
    (should (equal "Background:" (ecukes-output-background)))
    (should (equal ecukes-output-offset 2))))

(ert-deftest output-header ()
  (let ((ecukes-output-offset 0))
    (should (equal "Header:" (ecukes-output-header "Header")))
    (should (equal ecukes-output-offset 2))))

;; (ert-deftest output-step ()
;;   ;; TODO
;;   )

(ert-deftest output-newline ()
  (should (equal "" (ecukes-output-newline))))

(ert-deftest output-white ()
  (let ((text "text"))
    (should (equal "\e[37mtext\e[0m" (ecukes-output-white text)))))

(ert-deftest output-red ()
  (let ((text "text"))
    (should (equal "\e[31mtext\e[0m" (ecukes-output-red text)))))

(ert-deftest output-green ()
  (let ((text "text"))
    (should (equal "\e[32mtext\e[0m" (ecukes-output-green text)))))
