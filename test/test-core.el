;;; test-core.el --- Core functionality tests

;;; Commentary:
;; Tests for basic KB operations: assert, query, reasoning.

;;; Code:

(require 'ert)
(require 'kb-advanced-system)

(ert-deftest test-kb-init ()
  "Test that KB initialization works."
  (kb-init)
  (should (kb-get-microtheory 'BaseMt))
  (should (kb-get-microtheory 'CommonSenseMt)))

(ert-deftest test-basic-assert-query ()
  "Test basic assertion and querying."
  (kb-init)
  (kb-assert 'Socrates 'is-a 'human)
  (let ((result (kb-query 'Socrates 'is-a)))
    (should result)
    (should (member 'human result))))

(ert-deftest test-inheritance-reasoning ()
  "Test inheritance through is-a relations."
  (kb-init)
  (kb-assert 'Socrates 'is-a 'human)
  (kb-assert 'human 'is-a 'mammal)
  (kb-reason)
  (let ((result (kb-ask '(Socrates is-a))))
    (should (member 'human result))
    (should (member 'mammal result))))

(ert-deftest test-multiple-facts ()
  "Test storing and retrieving multiple facts."
  (kb-init)
  (kb-assert 'Socrates 'is-a 'human)
  (kb-assert 'Socrates 'nationality 'Greek)
  (kb-assert 'Plato 'is-a 'human)
  
  (should (member 'human (kb-query 'Socrates 'is-a)))
  (should (member 'Greek (kb-query 'Socrates 'nationality)))
  (should (member 'human (kb-query 'Plato 'is-a))))

(ert-deftest test-kb-status ()
  "Test that kb-status returns meaningful information."
  (kb-init)
  (kb-assert 'test 'fact 'value)
  (let ((status (kb-status)))
    (should status)
    (should (stringp status))))

;;; test-core.el ends here
