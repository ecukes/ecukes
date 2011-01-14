(ert-deftest template-substitute-with-single-substitution ()
  "Should substitute single substitution."
  (should
   (equal
    (ecukes-template-substitute
     "replace {foo} in string"
     '(("foo" . "bar")))
    "replace bar in string")))

(ert-deftest template-substitute-with-multiple-substitutions ()
  "Should substitute multiple substitutions."
  (should
   (equal
    (ecukes-template-substitute
     "replace {foo} and {baz} in string"
     '(("foo" . "bar")
       ("baz" . "qux")))
    "replace bar and qux in string")))

(ert-deftest template-dont-substitute-when-no-substitutions ()
  "Should return same string when no substitutions."
  (should
   (equal
    (ecukes-template-substitute
     "do not replace anything"
     '(("foo" . "bar")))
    "do not replace anything")))
