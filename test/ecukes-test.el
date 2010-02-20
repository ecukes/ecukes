(ert-deftest pass-no-feature-file ()
  (let ((feature-files (ecukes-init-feature-files (list))))
    (should-not feature-files)))

(ert-deftest pass-feature-file-that-does-no-exist ()
  (let ((feature-files (ecukes-init-feature-files (list "does not exist"))))
    (should-not feature-files)))

(ert-deftest pass-single-feature-file ()
  (with-mock
    (stub file-exists-p => t)
    (let ((feature-files (ecukes-init-feature-files (list "test.feature"))))
      (should (string-match-p "test\\.feature$" (car feature-files))))))

(ert-deftest pass-directory-of-features ()
  (with-mock
    (stub file-directory-p => t)
    (stub directory-files => (list "test1.feature" "test2.feature"))
    (let ((feature-files (ecukes-init-feature-files (list "features"))))
      (should (string-match-p "test2\\.feature$" (car feature-files)))
      (should (string-match-p "test1\\.feature$" (car (cdr feature-files)))))))
