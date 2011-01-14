(defmacro with-line (line &rest body)
  `(with-mock
    (stub buffer-substring => ,line)
    ,@body))

(ert-deftest parse-line-only-text ()
  (with-line
   "Given a known state"
   (should (equal (ecukes-parse-line) "Given a known state"))))

(ert-deftest parse-line-empty ()
  (with-line
   ""
   (should-not (ecukes-parse-line))))

(ert-deftest parse-line-with-whitespace ()
  (with-line
   "  Given a known state  "
   (should (equal (ecukes-parse-line) "  Given a known state  "))))

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
