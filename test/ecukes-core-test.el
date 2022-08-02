(require 'ecukes-core)


;;;; message

(ert-deftest ecukes-core/message-internal ()
  (let (ecukes-message-log ecukes-internal-message-log ecukes-verbose (ecukes-message t))
    (message "foo")
    (should (equal (car (car ecukes-internal-message-log)) 'message))
    (should (equal (cdr (car ecukes-internal-message-log)) "foo\n"))
    (should-not (-contains? ecukes-message-log "foo\n"))))

(ert-deftest ecukes-core/message-external ()
  (let (ecukes-message-log ecukes-internal-message-log ecukes-verbose ecukes-message)
    (message "foo")
    (should-not (equal (car (car ecukes-internal-message-log)) 'message))
    (should-not (equal (cdr (car ecukes-internal-message-log)) "foo\n"))
    (should (-contains? ecukes-message-log "foo\n"))))

(ert-deftest ecukes-core/message-verbose ()
  (let (ecukes-message-log ecukes-internal-message-log (ecukes-verbose t) ecukes-message)
    (message "foo")
    (should (equal (car (car ecukes-internal-message-log)) 'message))
    (should (equal (cdr (car ecukes-internal-message-log)) "foo\n"))
    (should (-contains? ecukes-message-log "foo\n"))))

(ert-deftest ecukes-core/message-non-verbose ()
  (let (ecukes-message-log ecukes-internal-message-log ecukes-verbose ecukes-message)
    (message "foo")
    (should-not (equal (car (car ecukes-internal-message-log)) 'message))
    (should-not (equal (cdr (car ecukes-internal-message-log)) "foo\n"))
    (should (-contains? ecukes-message-log "foo\n"))))

(ert-deftest ecukes-core/message-nil ()
  (let (ecukes-internal-message-log)
    (message nil)
    (should (equal (car (car ecukes-internal-message-log)) 'message))
    (should (equal (cdr (car ecukes-internal-message-log)) "\n"))))

(ert-deftest ecukes-core/format ()
  (let (ecukes-internal-message-log)
    (message "foo %s" "bar")
    (should (equal (car (car ecukes-internal-message-log)) 'message))
    (should (equal (cdr (car ecukes-internal-message-log)) "foo bar\n"))))


;;;;princ

(ert-deftest ecukes-core/princ-internal ()
  (let (ecukes-message-log ecukes-internal-message-log ecukes-verbose (ecukes-message t))
    (princ "foo")
    (should (equal (car (car ecukes-internal-message-log)) 'princ))
    (should (equal (cdr (car ecukes-internal-message-log)) "foo"))
    (should-not (-contains? ecukes-message-log "foo"))))

(ert-deftest ecukes-core/princ-external ()
  (let (ecukes-message-log ecukes-internal-message-log ecukes-verbose ecukes-message)
    (princ "foo")
    (should-not (equal (car (car ecukes-internal-message-log)) 'princ))
    (should-not (equal (cdr (car ecukes-internal-message-log)) "foo"))
    (should (-contains? ecukes-message-log "foo"))))

(ert-deftest ecukes-core/princ-verbose ()
  (let (ecukes-message-log ecukes-internal-message-log (ecukes-verbose t) ecukes-message)
    (princ "foo")
    (should (equal (car (car ecukes-internal-message-log)) 'princ))
    (should (equal (cdr (car ecukes-internal-message-log)) "foo"))
    (should (-contains? ecukes-message-log "foo"))))

(ert-deftest ecukes-core/princ-non-verbose ()
  (let (ecukes-message-log ecukes-internal-message-log ecukes-verbose ecukes-message)
    (princ "foo")
    (should-not (equal (car (car ecukes-internal-message-log)) 'princ))
    (should-not (equal (cdr (car ecukes-internal-message-log)) "foo"))
    (should (-contains? ecukes-message-log "foo"))))

(ert-deftest ecukes-core/princ-nil ()
  (let (ecukes-internal-message-log)
    (princ nil)
    (should (equal (car (car ecukes-internal-message-log)) 'princ))
    (should (equal (cdr (car ecukes-internal-message-log)) ""))))


;;;; print

(ert-deftest ecukes-core/print ()
  (let (ecukes-message-log ecukes-internal-message-log (ecukes-message t))
    (print "foo")
    (should (equal (car (car ecukes-internal-message-log)) 'print))
    (should (equal (cdr (car ecukes-internal-message-log)) "foo"))
    (should-not (-contains? ecukes-message-log "foo"))))


;;;; quit

(ert-deftest ecukes-core/quit-success ()
  (with-mock
   (stub getenv => nil)
   (mock (kill-emacs 0) :times 1)
   (ecukes-quit 0)))

(ert-deftest ecukes-core/quit-failure ()
  (with-mock
   (stub getenv => nil)
   (mock (kill-emacs 1) :times 1)
   (ecukes-quit)))

(ert-deftest ecukes-core/quit-graphical ()
  (with-mock
   (let ((ecukes-internal-message-log '((print . "foo") (princ . "bar") (message . "baz"))))
     (stub getenv => "/tmp/ecukes.XYZ")
     (mock (kill-emacs 1) :times 1)
     (mock (f-write-text "\"foo\"barbaz" 'utf-8 "/tmp/ecukes.XYZ") :times 1)
     (ecukes-quit))))
