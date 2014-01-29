(require 'ecukes-parse)

(ert-deftest parse-regular-step-given ()
  "Should parse given step."
  (with-parse-step
   "given"
   (lambda (name head body type arg)
     (should (eq type 'regular))
     (should (equal name "Given a known state"))
     (should (equal body "a known state")))))

(ert-deftest parse-regular-step-when ()
  "Should parse when step."
  (with-parse-step
   "when"
   (lambda (name head body type arg)
     (should (eq type 'regular))
     (should (equal name "When the key action"))
     (should (equal body "the key action")))))

(ert-deftest parse-regular-step-then ()
  "Should parse then step."
  (with-parse-step
   "then"
   (lambda (name head body type arg)
     (should (eq type 'regular))
     (should (equal name "Then observe outcomes"))
     (should (equal body "observe outcomes")))))

(ert-deftest parse-regular-step-and ()
  "Should parse and step."
  (with-parse-step
   "and"
   (lambda (name head body type arg)
     (should (eq type 'regular))
     (should (equal name "And another key action"))
     (should (equal body "another key action")))))

(ert-deftest parse-regular-step-but ()
  "Should parse but step."
  (with-parse-step
   "but"
   (lambda (name head body type arg)
     (should (eq type 'regular))
     (should (eq type 'regular))
     (should (equal name "But observe outcomes")))))

(ert-deftest parse-regular-step-concise-body-simple-keywords ()
  "Should parse concise body."
  (with-parse-step
   "body-simple-keywords"
   (lambda (name head body type arg)
     (should (eq type 'regular))
     (should (equal name "And input TEXT text another NAME and POS and MODE"))
     (should (equal body (format "input \"\\(.+\\)\" text another \"\\(.+\\)\" and \\([0-9]+\\) and \\(.+\\)"))))))

(ert-deftest parse-regular-step-concise-body-several-same-type-keywords ()
  "Should parse concise body."
  (with-parse-step
   "body-several-same-type-keywords"
   (lambda (name head body type arg)
     (should (eq type 'regular))
     (should (equal name "And input TEXT text another TEXT and TEXT"))
     (should (equal body (format "input \"\\(.+\\)\" text another \"\\(.+\\)\" and \"\\(.+\\)\""))))))

(ert-deftest parse-regular-step-concise-body-compound-keywords ()
  "Should parse concise body."
  (with-parse-step
   "body-compound-keywords"
   (lambda (name head body type arg)
     (should (eq type 'regular))
     (should (equal name "And select TEST-NAME in opened file LINE-POSITION and BUFFER-TEXT"))
     (should (equal body "select \"\\(.+\\)\" in opened file \\([0-9]+\\) and \"\\(.+\\)\"")))))

(ert-deftest parse-regular-step-concise-body-contents ()
  "Should parse concise body."
  (with-parse-step
   "body-contents"
   (lambda (name head body type arg)
     (should (eq type 'regular))
     (should (equal name "And input CONTENTS and CONTENTS and CONTENTS with action"))
     (should (equal body "input\\(?: \"\\(.*\\)\"\\|:\\) and\\(?: \"\\(.*\\)\"\\|:\\) and\\(?: \"\\(.*\\)\"\\|:\\) with action")))))

(ert-deftest parse-regular-step-concise-body-any-keyword ()
  "Should parse concise body."
  (with-parse-step
   "body-any-keyword"
   (lambda (name head body type arg)
     (should (eq type 'regular))
     (should (equal name "And input LANGUAGE and DIRECTION with action"))
     (should (equal body "input \"\\(.+\\)\" and \"\\(.+\\)\" with action")))))
