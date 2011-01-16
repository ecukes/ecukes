(defmacro with-line (line &rest body)
  `(with-mock
    (stub buffer-substring => ,line)
    ,@body))

(defun should-get-exact-line-when-no-strip (line)
  "Parsing a line when no strip should return that line exactly as it is."
  (with-line line (should (equal (ecukes-parse-line) line))))

(ert-deftest parse-line-only-text ()
  (should-get-exact-line-when-no-strip "Given a known state"))

(ert-deftest parse-line-empty ()
  (should-get-exact-line-when-no-strip ""))

(ert-deftest parse-line-with-whitespace ()
  (should-get-exact-line-when-no-strip "  Given a known state  "))

(ert-deftest parse-line-only-whitespace ()
  (with-line
   "  "
   (should (equal (ecukes-parse-line) "  "))))

(ert-deftest parse-line-strip-only-text ()
  (with-line
   "Given a known state"
   (should (equal (ecukes-parse-line t) "Given a known state"))))

(ert-deftest parse-line-strip-empty ()
  (with-line
   ""
   (should-not (ecukes-parse-line t))))

(ert-deftest parse-line-strip-with-whitespace ()
  (with-line
   "  Given a known state  "
   (should (equal (ecukes-parse-line t) "Given a known state"))))

(ert-deftest parse-line-strip-only-whitespace ()
  (with-line
   "  "
   (should-not (ecukes-parse-line t))))
