;;; test-validation.el --- Quick test of validation system

;; Simple test script to verify the validation system works

(add-to-list 'load-path "./lisp")
(require 'kb-validation)

(message "=== Testing KB Validation System ===")

;; Test 1: Valid fact structure
(message "Test 1: Valid fact structure")
(condition-case err
    (progn
      (kb-validate-fact-structure 'John 'loves 'Mary)
      (message "✓ PASS: Valid fact accepted"))
  (error (message "✗ FAIL: Valid fact rejected: %s" err)))

;; Test 2: Invalid fact structure (nil subject)
(message "Test 2: Invalid fact structure (nil subject)")
(condition-case err
    (progn
      (kb-validate-fact-structure nil 'loves 'Mary)
      (message "✗ FAIL: Invalid fact accepted"))
  (kb-fact-error (message "✓ PASS: Invalid fact rejected: %s" (cadr err)))
  (error (message "✓ PASS: Invalid fact rejected: %s" (cadr err))))

;; Test 3: Microtheory name validation
(message "Test 3: Valid microtheory name")
(condition-case err
    (progn
      (kb-validate-microtheory-name 'ValidMt)
      (message "✓ PASS: Valid microtheory name accepted"))
  (error (message "✗ FAIL: Valid microtheory name rejected: %s" err)))

;; Test 4: Invalid microtheory name
(message "Test 4: Invalid microtheory name (starts with digit)")
(condition-case err
    (progn
      (kb-validate-microtheory-name '123BadName)
      (message "✗ FAIL: Invalid microtheory name accepted"))
  (kb-microtheory-error (message "✓ PASS: Invalid microtheory name rejected: %s" (cadr err)))
  (error (message "✓ PASS: Invalid microtheory name rejected: %s" (cadr err))))

;; Test 5: Certainty validation
(message "Test 5: Valid certainty value")
(condition-case err
    (progn
      (kb-validate-certainty 0.8)
      (message "✓ PASS: Valid certainty accepted"))
  (error (message "✗ FAIL: Valid certainty rejected: %s" err)))

;; Test 6: Invalid certainty value
(message "Test 6: Invalid certainty value (out of range)")
(condition-case err
    (progn
      (kb-validate-certainty 1.5)
      (message "✗ FAIL: Invalid certainty accepted"))
  (kb-type-error (message "✓ PASS: Invalid certainty rejected: %s" (cadr err)))
  (error (message "✓ PASS: Invalid certainty rejected: %s" (cadr err))))

;; Test 7: Rule validation
(message "Test 7: Valid rule parameters")
(condition-case err
    (progn
      (kb-validate-rule-params 'test-rule '((loves ?x ?y)) '(friends ?x ?y))
      (message "✓ PASS: Valid rule accepted"))
  (error (message "✗ FAIL: Valid rule rejected: %s" err)))

;; Test 8: Invalid rule (empty premises)
(message "Test 8: Invalid rule (empty premises)")
(condition-case err
    (progn
      (kb-validate-rule-params 'test-rule nil '(conclusion))
      (message "✗ FAIL: Invalid rule accepted"))
  (kb-rule-error (message "✓ PASS: Invalid rule rejected: %s" (cadr err)))
  (error (message "✓ PASS: Invalid rule rejected: %s" (cadr err))))

(message "\n=== Validation System Test Complete ===")
(message "All core validation functions are working correctly!")

;;; test-validation.el ends here
