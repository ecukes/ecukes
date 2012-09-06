(ert-deftest find-project-dir-in-root ()
  "Should find project dir when we're visiting a buffer in a ecukes project root directory"
  (progn
    (should (equal (file-name-directory (or load-file-name buffer-file-name))
                   (ecukes-find-project-dir)))))

(ert-deftest find-project-dir-in-subdirectory ()
  "Should find project dir when we're visiting a buffer in a ecukes project sub directory"
  (with-mock
   (stub ecukes-cwd => (concat (file-name-directory (or load-file-name buffer-file-name)) "util"))
   (should (equal (file-name-directory (or load-file-name buffer-file-name))
                  (ecukes-find-project-dir)))))

(ert-deftest not-in-ecukes-project ()
  "Should fail gracefully when visiting a buffer outside of a ecukes project"
  (with-mock
    (stub ecukes-cwd => (ecukes-parent-dir
                         (file-name-directory (or load-file-name buffer-file-name))))
    (should-not (ecukes-find-project-dir))))
