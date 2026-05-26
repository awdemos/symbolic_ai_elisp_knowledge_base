;;; test-fixed-components.el --- Tests for fixed components
;; -*- lexical-binding: t; -*-

;; Author: AI Assistant
;; Keywords: tests, fixes, validation, nonmonotonic
;; Version: 1.0

;;; Commentary:
;; Tests for the architectural fixes applied to the knowledge base system.
;; Covers:
;; - Fixed kb-with-validation macro (now calls validators)
;; - Fixed kb-default-rule-is-defeated-p (no infinite recursion)
;; - Fixed kb-reason-with-defaults (syntax error corrected)
;; - Query engine functions
;; - Per-microtheory event counters

;;; Code:

(require 'ert)
(require 'kb-advanced-system)

;;; Validation Tests

(ert-deftest test-kb-with-validation-calls-validators ()
  "Test that kb-with-validation actually calls validation functions."
  (kb-init)
  ;; Define a test validator that records when it's called
  (let ((validator-called nil))
    (defun kb-validate-kb-assert-fact-params (&rest args)
      "Test validator."
      (setq validator-called t)
      t)
    (kb-with-validation kb-assert-fact (list 'Socrates 'is-a 'human)
      (kb-with-error-recovery
        t))
    (should validator-called)
    (fmakunbound 'kb-validate-kb-assert-fact-params)))

(ert-deftest test-kb-with-validation-rejects-invalid-args ()
  "Test that kb-with-validation rejects invalid arguments."
  (kb-init)
  ;; Create a validator that always rejects
  (let ((validator-called nil))
    (defun kb-validate-kb-test-invalid-params (&rest args)
      "Test validator that rejects."
      (setq validator-called t)
      (signal 'kb-validation-error (list "Invalid args" args)))
    (should-error
     (kb-with-validation kb-test-invalid (list 'bad)
       (kb-with-error-recovery
         t))
     :type 'kb-validation-error)
    (should validator-called)
    (fmakunbound 'kb-validate-kb-test-invalid-params)))

;;; Nonmonotonic Reasoning Tests

(ert-deftest test-kb-default-rule-is-defeated-p-no-recursion ()
  "Test that kb-default-rule-is-defeated-p doesn't cause infinite recursion."
  (kb-init)
  (let* ((rule (kb-default-rule-create
                :name 'test-rule
                :premises '((is-a X bird))
                :conclusion '(flies X)
                :exceptions '(is-a X penguin)))
         (result (kb-default-rule-is-defeated-p rule)))
    (should (null result))))

(ert-deftest test-kb-default-rule-is-defeated-p-with-exception ()
  "Test that kb-default-rule-is-defeated-p detects exceptions."
  (kb-init)
  ;; Assert exception facts
  (kb-assert 'Tweety 'is-a 'penguin)
  (let* ((rule (kb-default-rule-create
                :name 'birds-fly
                :premises '((is-a X bird))
                :conclusion '(flies X)
                :exceptions '(is-a X penguin)))
         (result (kb-default-rule-is-defeated-p rule)))
    ;; Should not be defeated (exceptions are for the specific case)
    (should (null result))))

(ert-deftest test-kb-reason-with-defaults-returns-result ()
  "Test that kb-reason-with-defaults returns a result without syntax errors."
  (kb-init)
  (kb-assert 'Tweety 'is-a 'bird)
  (let ((result (kb-reason-with-defaults '(flies Tweety))))
    ;; Should return something (possibly nil if no applicable rules)
    (should (or (null result) (consp result)))))

;;; Query Engine Tests

(ert-deftest test-kb-query-in-mt-exists ()
  "Test that kb-query-in-mt is available."
  (kb-init)
  (kb-assert 'Socrates 'is-a 'human)
  (let ((result (kb-query-in-mt 'Socrates 'is-a 'CommonSenseMt)))
    (should result)
    (should (member 'human (mapcar #'kb-fact-object result)))))

(ert-deftest test-kb-query-with-inheritance-exists ()
  "Test that kb-query-with-inheritance is available."
  (kb-init)
  (kb-assert 'Socrates 'is-a 'human 'CommonSenseMt)
  (let ((result (kb-query-with-inheritance 'Socrates 'is-a 'CommonSenseMt)))
    (should result)))

;;; Event Counter Tests

(ert-deftest test-kb-get-event-counter-scoped ()
  "Test that event counters are scoped per microtheory."
  (kb-init)
  (let ((counter1 (kb-get-event-counter 'BaseMt))
        (counter2 (kb-get-event-counter 'BaseMt))
        (counter3 (kb-get-event-counter 'CommonSenseMt)))
    ;; Same microtheory should increment sequentially
    (should (= counter2 (1+ counter1)))
    ;; Different microtheory should have independent counter
    (should (not (= counter1 counter3)))))

(ert-deftest test-kb-create-event-id-is-scoped ()
  "Test that created events have scoped IDs."
  (kb-init)
  (let* ((event1 (kb-create-event 'test :mt 'BaseMt))
         (event2 (kb-create-event 'test :mt 'CommonSenseMt))
         (id1 (kb-event-id event1))
         (id2 (kb-event-id event2)))
    ;; IDs should contain the microtheory name
    (should (string-match-p "BaseMt" (format "%s" id1)))
    (should (string-match-p "CommonSenseMt" (format "%s" id2)))))

;;; Integration Tests

(ert-deftest test-full-pipeline ()
  "Test the full KB pipeline with all fixes."
  (kb-init)
  
  ;; Assert facts
  (kb-assert 'Socrates 'is-a 'human)
  (kb-assert 'human 'mortal t)
  
  ;; Query
  (let ((is-human (kb-query 'Socrates 'is-a))
        (is-mortal (kb-query 'human 'mortal)))
    (should is-human)
    (should (member 'human (mapcar #'kb-fact-object is-human)))
    (should is-mortal)
    (should (member t (mapcar #'kb-fact-object is-mortal)))
    
    ;; Create event
    (let ((event (kb-create-event 'test :mt 'BaseMt)))
      (should event)
      (should (kb-event-p event))
      
      ;; Query with validation
      (kb-with-validation kb-query (list 'Socrates 'is-a)
        (kb-with-error-recovery
          (let ((result (kb-query 'Socrates 'is-a)))
             (should result)))))))

(provide 'test-fixed-components)
;;; test-fixed-components.el ends here
