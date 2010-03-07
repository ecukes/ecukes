(ert-deftest ecukes-dump-background ()
  (let ((background (mock-background)))
    ;; Dump the background
    (ecukes-dump-background background)

    ;; Make sure the background was dumped
    (should (file-exists-p ecukes-dump-background-file))
    (with-temp-buffer
      (insert-file-contents-literally ecukes-dump-background-file)
      (should (equal "[cl-struct-ecukes-background ([cl-struct-ecukes-step \"Given I have this or that\" nil])]" (buffer-substring-no-properties (point-min) (point-max)))))

    ;; Read the background and make sure it's correct
    (let ((step (car (ecukes-background-steps (ecukes-dump-read-background)))))
      (should (equal "Given I have this or that" (ecukes-step-name step))))

    ;; Delete the background
    (ecukes-dump-delete-background)

    ;; Make sure the background is deleted
    (should-not (file-exists-p ecukes-dump-background-file))))

(ert-deftest ecukes-dump-scenario ()
  (let ((scenario (mock-scenario)))
    ;; Dump the scenario
    (ecukes-dump-scenario scenario)

    ;; Make sure the scenario was dumped
    (should (file-exists-p ecukes-dump-scenario-file))
    (with-temp-buffer
      (insert-file-contents-literally ecukes-dump-scenario-file)
      (should (equal "[cl-struct-ecukes-scenario \"Addition\" ([cl-struct-ecukes-step \"Given I have this or that\" nil])]" (buffer-substring-no-properties (point-min) (point-max)))))

    ;; Read the scenario and make sure it's correct
    (let ((step (car (ecukes-scenario-steps (ecukes-dump-read-scenario)))))
      (should (equal "Given I have this or that" (ecukes-step-name step))))

    ;; Delete the scenario
    (ecukes-dump-delete-scenario)

    ;; Make sure the scenario is deleted
    (should-not (file-exists-p ecukes-dump-scenario-file))))
 
