;;; ecukes-dump.el --- Helpers for dumping and reading ecukes objects

(defconst ecukes-dump-background-file "background.dump"
  "Background dump file.")

(defconst ecukes-dump-scenario-file "scenario.dump"
  "Scenario dump file.")

(defconst ecukes-dump-offset-file "offset.dump"
  "Offset dump file.")


(defun ecukes-dump-background (background)
  "Dumps BACKGROUND to `ecukes-dump-background-file'."
  (ecukes-dump-object background ecukes-dump-background-file))

(defun ecukes-dump-scenario (scenario)
  "Dumps SCENARIO to `ecukes-dump-scenario-file'."
  (ecukes-dump-object scenario ecukes-dump-scenario-file))

(defun ecukes-dump-offset (offset)
  "Dumps OFFSET to `ecukes-dump-offset-file'."
  (ecukes-dump-object offset ecukes-dump-offset-file))

(defun ecukes-dump-object (object file)
  "Dumps OBJECT to FILE."
  (with-temp-file file
    (prin1 object (current-buffer))))

(defun ecukes-dump-delete-background ()
  "Deletes background dump file."
  (delete-file ecukes-dump-background-file))

(defun ecukes-dump-delete-scenario ()
  "Deletes scenario dump file."
  (delete-file ecukes-dump-scenario-file))

(defun ecukes-dump-delete-offset ()
  "Deletes offset dump file."
  (delete-file ecukes-dump-offset-file))

(defun ecukes-dump-read-background ()
  "Reads background from file."
  (ecukes-dump-read-object ecukes-dump-background-file))

(defun ecukes-dump-read-scenario ()
  "Reads scenario from file."
  (ecukes-dump-read-object ecukes-dump-scenario-file))

(defun ecukes-dump-read-offset ()
  "Reads offset from file."
  (ecukes-dump-read-object ecukes-dump-offset-file))

(defun ecukes-dump-read-object (file)
  "Reads object from file."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (read (buffer-substring-no-properties (point-min) (point-max)))))

(provide 'ecukes-dump)

;;; ecukes-dump.el ends here
