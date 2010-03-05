;;; ecukes-dump.el --- Helpers for dumping and reading ecukes objects

(defconst ecukes-dump-background-file "background.dump"
  "Dump file for background.")

(defconst ecukes-dump-scenario-file "scenario.dump"
  "Dump file for scenario.")

(defun ecukes-dump-background (background)
  "Dumps BACKGROUND to `ecukes-dump-background-file'."
  (ecukes-dump-object background ecukes-dump-background-file))

(defun ecukes-dump-scenario (scenario)
  "Dumps SCENARIO to `ecukes-dump-scenario-file'."
  (ecukes-dump-object scenario ecukes-dump-scenario-file))

(defun ecukes-dump-object (object file)
  "Dumps OBJECT to FILE."
  (with-temp-file file
    (princ object (current-buffer))))

(defun ecukes-dump-delete-background ()
  "Deletes background dump file."
  (delete-file ecukes-dump-background-file))

(defun ecukes-dump-delete-scenario ()
  "Deletes scenario dump file."
  (delete-file ecukes-dump-scenario-file))

(provide 'ecukes-dump)

;;; ecukes-dump.el ends here
