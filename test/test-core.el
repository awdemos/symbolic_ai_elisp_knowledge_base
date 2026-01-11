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
    (should (member 'human (mapcar #'kb-fact-object result)))))

(ert-deftest test-inheritance-reasoning ()
  "Test basic fact assertion and querying."
  (kb-init)
  ;; Add direct facts
  (kb-assert 'Socrates 'is-a 'human)
  (kb-assert 'Socrates 'mortal t)
  (kb-assert 'human 'mortal t)
  
  ;; Query direct assertions
  (let ((is-human (kb-query 'Socrates 'is-a))
        (is-mortal (kb-query 'Socrates 'mortal))
        (human-mortal (kb-query 'human 'mortal)))
    ;; Verify Socrates facts
    (should is-human)
    (should (member 'human (mapcar #'kb-fact-object is-human)))
    (should is-mortal)
    (should (member t (mapcar #'kb-fact-object is-mortal)))
    ;; Verify human fact
    (should human-mortal)
    (should (member t (mapcar #'kb-fact-object human-mortal)))))

(ert-deftest test-multiple-facts ()
  "Test storing and retrieving multiple facts."
  (kb-init)
  (kb-assert 'Socrates 'is-a 'human)
  (kb-assert 'Socrates 'nationality 'Greek)
  (kb-assert 'Plato 'is-a 'human)
  
  (should (member 'human (mapcar #'kb-fact-object (kb-query 'Socrates 'is-a))))
  (should (member 'Greek (mapcar #'kb-fact-object (kb-query 'Socrates 'nationality))))
  (should (member 'human (mapcar #'kb-fact-object (kb-query 'Plato 'is-a)))))

(ert-deftest test-kb-status ()
  "Test that kb-status returns meaningful information."
  (kb-init)
  (kb-assert 'test 'fact 'value)
  (let ((status (kb-status)))
    (should status)
    (should (stringp status))))

;;; test-core.el ends here
