(require 'ecukes-reporter)

(ert-deftest ecukes-reporter-test/no-indent ()
  (with-mock
   (mock (princ "foo"))
   (ecukes-reporter-print "foo")))

(ert-deftest ecukes-reporter-test/no-indent-with-objects ()
  (with-mock
   (mock (princ "foo bar"))
   (ecukes-reporter-print "foo %s" "bar")))

(ert-deftest ecukes-reporter-test/with-indent ()
  (with-mock
   (mock (princ "  foo"))
   (ecukes-reporter-print 2 "foo")))

(ert-deftest ecukes-reporter-test/with-indent-and-objects ()
  (with-mock
   (mock (princ "  foo bar"))
   (ecukes-reporter-print 2 "foo %s" "bar")))
