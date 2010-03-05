(ert-deftest ecukes-dump-background ()
  (let ((background (mock-background)))
    (ecukes-dump-background background)
    (should (file-exists-p ecukes-dump-background-file))
    (with-temp-buffer
      (insert-file-contents-literally ecukes-dump-background-file)
      (should (equal "[cl-struct-ecukes-background ([cl-struct-ecukes-step Given I have this or that nil])]" (buffer-substring-no-properties (point-min) (point-max)))))
    (ecukes-dump-delete-background)
    (should-not (file-exists-p ecukes-dump-background-file))))

(ert-deftest ecukes-dump-scenario ()
  (let ((scenario (mock-scenario)))
    (ecukes-dump-scenario scenario)
    (should (file-exists-p ecukes-dump-scenario-file))
    (with-temp-buffer
      (insert-file-contents-literally ecukes-dump-scenario-file)
      (should (equal "[cl-struct-ecukes-scenario Addition ([cl-struct-ecukes-step Given I have this or that nil])]" (buffer-substring-no-properties (point-min) (point-max)))))
    (ecukes-dump-delete-scenario)
    (should-not (file-exists-p ecukes-dump-scenario-file))))
