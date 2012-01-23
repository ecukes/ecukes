(ert-deftest parse-table-step-single-row ()
  "Should parse table with single row."
  (with-parse-step
   "table-single-row"
   (lambda (name type arg)
     (should (eq type 'table))
     (should (equal arg '(("meal" "price") ("Hamburger" "$4.50")))))))

(ert-deftest parse-table-step-multiple-rows ()
  "Should parse table with multiple rows."
  (with-parse-step
   "table-multiple-rows"
   (lambda (name type arg)
     (should (eq type 'table))
     (should (equal arg '(("meal" "price") ("Hamburger" "$4.50") ("Pizza" "$5.30")))))))

(ert-deftest parse-table-step-wrong-indentation ()
  "Should parse table with wrong indentation."
  (with-parse-step
   "table-wrong-indentation"
   (lambda (name type arg)
     (should (eq type 'table))
     (should (equal arg '(("meal" "price") ("Hamburger" "$4.50") ("Pizza" "$5.30")))))))

(ert-deftest parse-table-step-same-row ()
  "Should parse table with same row twice."
  (with-parse-step
   "table-same-row"
   (lambda (name type arg)
     (should (eq type 'table))
     (should (equal arg '(("meal" "price") ("Hamburger" "$4.50") ("Hamburger" "$4.50")))))))

(ert-deftest parse-table-step-empty-columns ()
  "Should parse table with empty columns."
  (with-parse-step
   "table-empty-columns"
   (lambda (name type arg)
     (should (eq type 'table))
     (should (equal arg '(("meal" "price") ("Hamburger" "") ("Pizza" "$5.30")))))))
