(ert-deftest startup-root-regex-unix ()
  (should (string-match-p ecukes-root-regex "/")))

(ert-deftest startup-root-regex-win ()
  (should (string-match-p ecukes-root-regex "c:/"))
  (should (string-match-p ecukes-root-regex "D:/")))

(ert-deftest startup-feature-files-no-arguments ()
  (let ((feature-files (ecukes-feature-files nil)))
    (should-not feature-files)))

(ert-deftest startup-feature-files-single-argument-does-not-exist ()
  (let ((feature-files (ecukes-feature-files '("does-not.exist"))))
    (should-not feature-files)))

(ert-deftest startup-feature-files-multiple-arguments-does-not-exist ()
  (let ((feature-files (ecukes-feature-files '("does-not.exist" "non-existing.file"))))
    (should-not feature-files)))

(ert-deftest startup-feature-files-valid-single-feature-file ()
  (with-mock
    (stub file-exists-p => t)
    (stub file-directory-p => nil)
    (let ((feature-files (ecukes-feature-files '("test.feature"))))
      (should (string-match-p "test\\.feature$" (car feature-files))))))

(ert-deftest startup-feature-files-valid-multiple-feature-files ()
  (with-mock
    (stub file-exists-p => t)
    (stub file-directory-p => nil)
    (let ((feature-files (ecukes-feature-files '("test1.feature" "test2.feature"))))
      (should (string-match-p "test1\\.feature$" (car feature-files)))
      (should (string-match-p "test2\\.feature$" (cadr feature-files))))))

(ert-deftest startup-pass-directory-of-features ()
  (with-mock
    (stub file-directory-p => t)
    (stub directory-files => (list "test1.feature" "test2.feature"))
    (let ((feature-files (ecukes-feature-files '("features"))))
      (should (string-match-p "test1\\.feature$" (car feature-files)))
      (should (string-match-p "test2\\.feature$" (cadr feature-files))))))

(ert-deftest startup-load-non-existing-project ()
  (with-mock
   (stub ecukes-features-root => nil)
   (track-output
    (quiet-message (ecukes-load-project nil))
    (should
     (member (ecukes-color-red "Could not find features root") message-output)))))
