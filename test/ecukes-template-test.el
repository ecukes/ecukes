(ert-deftest template-file-name ()
  (let ((ecukes-template-path "/path/to/templates"))
    (should
     (equal
      "/path/to/templates/template.tpl"
      (ecukes-template-file "template"))))) 

(ert-deftest template-replace-single-replacement ()
  (let* ((contents "replace {THIS} text")
         (replacement (ecukes-template-replace contents "THIS" "that")))
    (should (equal "replace that text" replacement))))

(ert-deftest template-replace-multiple-replacements ()
  (let* ((contents "replace {THIS} text and {THIS} text")
         (replacement (ecukes-template-replace contents "THIS" "that")))
    (should (equal "replace that text and that text" replacement))))

(ert-deftest template-replace-no-replacement ()
  (let* ((contents "Do not replace THIS text")
         (replacement (ecukes-template-replace contents "THIS" "that")))
    (should (equal contents replacement))))

(ert-deftest template-assign-single-assignment ()
  (let* ((contents "replace {THIS} text")
        (replacements '(("THIS" . "that")))
        (actual (ecukes-template-assign contents replacements)))
    (should (equal "replace that text" actual))))

(ert-deftest template-assign-multiple-assignments ()
  (let* ((contents "replace {THIS} text and {THAT} text")
        (replacements '(("THIS" . "that") ("THAT" . "this")))
        (actual (ecukes-template-assign contents replacements)))
    (should (equal "replace that text and this text" actual))))

(ert-deftest template-get-with-single-replacement ()
  (with-mock
   (stub ecukes-template-file => nil)
   (stub insert-file-contents-literally => nil)
   (stub buffer-substring-no-properties => "replace {THIS} text")
   (let* ((replacements '(("THIS" . "that")))
         (actual (ecukes-template-get "does not matter" replacements)))
     (should (equal "replace that text" actual)))))

(ert-deftest template-get-with-multiple-replacements ()
  (with-mock
   (stub ecukes-template-file => nil)
   (stub insert-file-contents-literally => nil)
   (stub buffer-substring-no-properties => "replace {THIS} text and {THAT} text")
   (let* ((replacements '(("THIS" . "that") ("THAT" . "this")))
         (actual (ecukes-template-get "does not matter" replacements)))
     (should (equal "replace that text and this text" actual)))))

(ert-deftest template-get-with-no-replacements ()
  (with-mock
   (let ((contents "Do not replace THIS text"))
     (stub ecukes-template-file => nil)
     (stub insert-file-contents-literally => nil)
     (stub buffer-substring-no-properties => contents)
     (let ((actual (ecukes-template-get "does not matter")))
       (should (equal contents actual))))))
