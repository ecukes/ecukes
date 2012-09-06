;;; ecukes-directories.el --- functions dealing with files and directories


(defun ecukes-find-project-dir ()
  "Start in current directory and check for features dir. If
  found then return dir path, otherwise traverse up until features dir is found"
  (interactive)
  (let ((cwd (ecukes-cwd))
        (project-dir nil))
    (progn
      (ecukes-recurse-up-directories 
       cwd
       '(lambda (dir) (if (and (not project-dir) (ecukes-find-features-dir dir))
                     (setq project-dir dir))))
      (if (or (null project-dir) (string-equal project-dir ""))
          nil
        (concat (ecukes-remove-last project-dir "/") "/")))))

(defun ecukes-find-features-dir (dir)
  "Return true if current directory contains a subdir named 'features'"
  (let ((files (directory-files dir)))
    (ecukes-contains-string "features" files)))

(defun ecukes-cwd ()
  "Return current working directory inside or outside of emacs"
  (file-name-directory (or load-file-name buffer-file-name)))

(defun ecukes-remove-last (s ending)
  "If string S ends with ENDING, remove it and return string. Otherwise return string."
  (let ((cwd s))
    (if (ecukes-ends-with cwd ending) 
      (substring cwd 0 (ecukes-ends-with cwd ending))
      cwd)))

(defun ecukes-parent-dir (currdir)
  "Return parent directory"
  (let ((remaining (butlast (split-string (ecukes-remove-last currdir "/") "/"))))
    (if (null remaining)
        remaining
      (mapconcat 'identity  remaining "/"))))

(defun ecukes-recurse-up-directories (curr-dir f)
  "Starting in CURR-DIR, recurse up the directories until you hit root calling F on each"
  (cond
   ((null curr-dir)
    nil)
   ((file-directory-p curr-dir)
    (progn
      (funcall f curr-dir)
      (ecukes-recurse-up-directories (ecukes-parent-dir curr-dir) f)))))

(defun ecukes-contains-string (string list)
  "Return STRING if LIST contains STRING, otherwise return nil"
  (find string list :test #'string-equal))

(defun ecukes-ends-with (s ending)
  "Check if S ends with ENDING. If it does, return index, else return null."
  (if (and (<= (length ending) (length s)) (string= (substring s (- 0 (length ending))) ending))
      (- (length s) (length ending))))


(provide 'ecukes-directories)

;;; ecukes-directories.el ends here
