;;; kb-testing.el --- Testing Framework for Knowledge Base System

;; Author: AI Assistant
;; Keywords: ai, testing, validation, framework
;; Version: 2.1

;;; Commentary:

;; This package provides a testing framework specifically designed for
;; knowledge base systems, with support for fact assertions, inference
;; testing, and microtheory validation.

;;; Code:

(require 'cl-lib)
(require 'kb-microtheories)
(require 'kb-inference-engine)

;;; Test Structures

(cl-defstruct (kb-test (:constructor kb-test-create)
                       (:copier nil))
  "A knowledge base test case."
  name                   ; test name
  description           ; test description
  setup-fn              ; function to set up test environment
  test-fn               ; main test function
  teardown-fn           ; cleanup function
  microtheory           ; test microtheory
  expected-results      ; expected outcomes
  actual-results        ; actual outcomes
  status                ; :not-run, :passed, :failed, :error
  error-message         ; error details if failed
  duration              ; execution time
  timestamp)            ; when test was run

(cl-defstruct (kb-test-suite (:constructor kb-test-suite-create)
                             (:copier nil))
  "A collection of related tests."
  name
  description
  tests                 ; list of kb-test objects
  setup-fn              ; suite-level setup
  teardown-fn           ; suite-level teardown
  status                ; :not-run, :passed, :failed, :partial
  results)              ; summary results

(cl-defstruct (kb-test-result (:constructor kb-test-result-create)
                              (:copier nil))
  "Test execution results."
  total-tests
  passed
  failed
  errors
  skipped
  duration
  start-time
  end-time)

;;; Variables

(defvar kb-test-suites (make-hash-table :test 'equal)
  "Hash table of all test suites.")

(defvar kb-test-current-suite nil
  "Currently executing test suite.")

(defvar kb-test-current-test nil
  "Currently executing test.")

(defvar kb-test-temp-microtheories nil
  "List of temporary microtheories created during testing.")

(defvar kb-test-verbose t
  "Whether to show verbose test output.")

;;; Test Definition Macros

(defmacro kb-deftest (name &rest body)
  "Define a knowledge base test."
  (declare (indent 1))
  (let* ((name-str (symbol-name name))
         (description (if (stringp (car body)) (pop body) name-str))
         (test-body body))
    `(let ((test (kb-test-create
                  :name ',name
                  :description ,description
                  :test-fn (lambda () ,@test-body)
                  :status :not-run
                  :microtheory (kb-create-temp-test-microtheory ',name))))
       (kb-register-test test))))

(defmacro kb-deftest-suite (name &rest body)
  "Define a test suite."
  (declare (indent 1))
  (let* ((name-str (symbol-name name))
         (description (if (stringp (car body)) (pop body) name-str))
         (suite-body body))
    `(let ((suite (kb-test-suite-create
                   :name ',name
                   :description ,description
                   :tests nil
                   :status :not-run)))
       (setq kb-test-current-suite suite)
       ,@suite-body
       (puthash ',name suite kb-test-suites)
       suite)))

(defmacro kb-with-test-microtheory (mt-name &rest body)
  "Execute test body in a temporary microtheory."
  `(let* ((temp-mt (kb-create-temp-test-microtheory ',mt-name))
          (kb-current-mt temp-mt))
     (unwind-protect
         (progn ,@body)
       (kb-cleanup-temp-microtheory temp-mt))))

;;; Test Assertion Functions

(defun kb-assert-true (condition &optional message)
  "Assert that CONDITION is true."
  (unless condition
    (error "Assertion failed%s: expected true, got %s" 
           (if message (concat " (" message ")") "")
           condition)))

(defun kb-assert-false (condition &optional message)
  "Assert that CONDITION is false."
  (when condition
    (error "Assertion failed%s: expected false, got %s"
           (if message (concat " (" message ")") "")
           condition)))

(defun kb-assert-equal (expected actual &optional message)
  "Assert that EXPECTED equals ACTUAL."
  (unless (equal expected actual)
    (error "Assertion failed%s: expected %S, got %S"
           (if message (concat " (" message ")") "")
           expected actual)))

(defun kb-assert-fact-exists (subject predicate object &optional mt)
  "Assert that a specific fact exists in the knowledge base."
  (let ((facts (kb-query-with-inheritance subject predicate mt)))
    (unless (cl-some (lambda (fact) (equal (kb-fact-object fact) object)) facts)
      (error "Fact assertion failed: %s %s %s not found" subject predicate object))))

(defun kb-assert-fact-not-exists (subject predicate object &optional mt)
  "Assert that a specific fact does not exist in the knowledge base."
  (let ((facts (kb-query-with-inheritance subject predicate mt)))
    (when (cl-some (lambda (fact) (equal (kb-fact-object fact) object)) facts)
      (error "Negative fact assertion failed: %s %s %s should not exist" 
             subject predicate object))))

(defun kb-assert-inference (premises conclusion &optional mt)
  "Assert that PREMISES logically imply CONCLUSION."
  (let ((kb-current-mt (or mt kb-current-mt)))
    ;; Add premises temporarily
    (dolist (premise premises)
      (apply #'kb-add-fact premise))
    
    ;; Perform inference
    (kb-reason mt)
    
    ;; Check if conclusion can be inferred
    (let ((result (kb-query-with-inheritance 
                   (car conclusion) (cadr conclusion) mt)))
      (unless (cl-some (lambda (fact) 
                        (equal (kb-fact-object fact) (caddr conclusion))) 
                      result)
        (error "Inference assertion failed: %s does not follow from %s" 
               conclusion premises)))))

(defun kb-assert-microtheory-consistent (mt-name)
  "Assert that a microtheory is logically consistent."
  (let ((inconsistencies (kb-find-microtheory-inconsistencies mt-name)))
    (when inconsistencies
      (error "Consistency assertion failed: %s has inconsistencies: %s"
             mt-name inconsistencies))))

;;; Test Utility Functions

(defun kb-create-temp-test-microtheory (test-name)
  "Create a temporary microtheory for testing."
  (let ((mt-name (intern (format "TestMt-%s-%d" test-name (random 10000)))))
    (kb-create-microtheory mt-name 'CommonSenseMt)
    (push mt-name kb-test-temp-microtheories)
    mt-name))

(defun kb-cleanup-temp-microtheory (mt-name)
  "Clean up a temporary test microtheory."
  (remhash mt-name kb-microtheories)
  (setq kb-test-temp-microtheories 
        (remove mt-name kb-test-temp-microtheories)))

(defun kb-find-microtheory-inconsistencies (mt-name)
  "Find logical inconsistencies in a microtheory."
  (let ((mt (kb-get-microtheory mt-name))
        (inconsistencies nil))
    (when mt
      ;; Simple consistency check - look for contradictory facts
      (maphash 
       (lambda (subject facts)
         (let ((predicates (make-hash-table :test 'equal)))
           (dolist (fact facts)
             (let ((pred (kb-fact-predicate fact))
                   (obj (kb-fact-object fact)))
               (let ((existing (gethash pred predicates)))
                 (cond
                  ((null existing)
                   (puthash pred obj predicates))
                  ((not (equal existing obj))
                   (push (format "%s %s has conflicting values: %s and %s"
                                subject pred existing obj)
                         inconsistencies))))))))
       (kb-microtheory-facts mt)))
    inconsistencies))

;;; Test Registration and Execution

(defun kb-register-test (test)
  "Register a test with the current suite."
  (if kb-test-current-suite
      (push test (kb-test-suite-tests kb-test-current-suite))
    (error "No current test suite - use kb-deftest-suite first")))

(defun kb-run-test (test)
  "Execute a single test."
  (setq kb-test-current-test test)
  (setf (kb-test-timestamp test) (current-time))
  (setf (kb-test-status test) :running)
  
  (let ((start-time (current-time))
        (original-mt kb-current-mt))
    
    (condition-case err
        (progn
          ;; Setup
          (when (kb-test-setup-fn test)
            (funcall (kb-test-setup-fn test)))
          
          ;; Set test microtheory
          (when (kb-test-microtheory test)
            (setq kb-current-mt (kb-test-microtheory test)))
          
          ;; Run test
          (funcall (kb-test-test-fn test))
          
          ;; If we get here, test passed
          (setf (kb-test-status test) :passed)
          (when kb-test-verbose
            (message "✓ %s" (kb-test-name test))))
      
      (error
       (setf (kb-test-status test) :failed)
       (setf (kb-test-error-message test) (error-message-string err))
       (when kb-test-verbose
         (message "✗ %s: %s" (kb-test-name test) (error-message-string err)))))
    
    ;; Cleanup
    (condition-case err
        (progn
          (when (kb-test-teardown-fn test)
            (funcall (kb-test-teardown-fn test)))
          (setq kb-current-mt original-mt))
      (error
       (message "Test cleanup failed for %s: %s" 
               (kb-test-name test) (error-message-string err))))
    
    ;; Record duration
    (setf (kb-test-duration test) 
          (float-time (time-subtract (current-time) start-time)))
    
    test))

(defun kb-run-test-suite (suite-name)
  "Execute all tests in a test suite."
  (interactive (list (completing-read "Test suite: " 
                                     (kb-list-test-suite-names))))
  (let* ((suite (gethash (intern suite-name) kb-test-suites))
         (start-time (current-time))
         (results (kb-test-result-create :start-time start-time
                                        :total-tests 0 :passed 0 
                                        :failed 0 :errors 0 :skipped 0)))
    
    (unless suite
      (error "Test suite %s not found" suite-name))
    
    (when kb-test-verbose
      (message "Running test suite: %s" (kb-test-suite-name suite)))
    
    ;; Suite setup
    (when (kb-test-suite-setup-fn suite)
      (funcall (kb-test-suite-setup-fn suite)))
    
    ;; Run each test
    (dolist (test (reverse (kb-test-suite-tests suite)))
      (cl-incf (kb-test-result-total-tests results))
      (kb-run-test test)
      
      (case (kb-test-status test)
        (:passed (cl-incf (kb-test-result-passed results)))
        (:failed (cl-incf (kb-test-result-failed results)))
        (:error (cl-incf (kb-test-result-errors results)))
        (t (cl-incf (kb-test-result-skipped results)))))
    
    ;; Suite teardown
    (when (kb-test-suite-teardown-fn suite)
      (funcall (kb-test-suite-teardown-fn suite)))
    
    ;; Cleanup temp microtheories
    (dolist (mt kb-test-temp-microtheories)
      (kb-cleanup-temp-microtheory mt))
    (setq kb-test-temp-microtheories nil)
    
    ;; Record results
    (setf (kb-test-result-end-time results) (current-time))
    (setf (kb-test-result-duration results)
          (float-time (time-subtract (current-time) start-time)))
    (setf (kb-test-suite-results suite) results)
    
    ;; Update suite status
    (setf (kb-test-suite-status suite)
          (cond
           ((> (kb-test-result-failed results) 0) :failed)
           ((> (kb-test-result-errors results) 0) :failed)
           ((= (kb-test-result-passed results) 
               (kb-test-result-total-tests results)) :passed)
           (t :partial)))
    
    ;; Display results
    (kb-display-test-results suite-name results)
    results))

(defun kb-run-all-tests ()
  "Run all registered test suites."
  (interactive)
  (let ((total-results (kb-test-result-create 
                       :start-time (current-time)
                       :total-tests 0 :passed 0 :failed 0 :errors 0 :skipped 0)))
    
    (maphash (lambda (suite-name suite)
               (let ((results (kb-run-test-suite (symbol-name suite-name))))
                 (cl-incf (kb-test-result-total-tests total-results) 
                          (kb-test-result-total-tests results))
                 (cl-incf (kb-test-result-passed total-results) 
                          (kb-test-result-passed results))
                 (cl-incf (kb-test-result-failed total-results) 
                          (kb-test-result-failed results))
                 (cl-incf (kb-test-result-errors total-results) 
                          (kb-test-result-errors results))
                 (cl-incf (kb-test-result-skipped total-results) 
                          (kb-test-result-skipped results))))
             kb-test-suites)
    
    (setf (kb-test-result-end-time total-results) (current-time))
    (setf (kb-test-result-duration total-results)
          (float-time (time-subtract (current-time) 
                                    (kb-test-result-start-time total-results))))
    
    (kb-display-test-results "ALL SUITES" total-results)
    total-results))

;;; Test Result Display

(defun kb-display-test-results (suite-name results)
  "Display test results."
  (let ((total (kb-test-result-total-tests results))
        (passed (kb-test-result-passed results))
        (failed (kb-test-result-failed results))
        (errors (kb-test-result-errors results))
        (duration (kb-test-result-duration results)))
    
    (message "\n=== Test Results: %s ===" suite-name)
    (message "Total: %d, Passed: %d, Failed: %d, Errors: %d, Duration: %.3fs"
             total passed failed errors duration)
    
    (when (or (> failed 0) (> errors 0))
      (kb-show-failed-tests suite-name))))

(defun kb-show-failed-tests (suite-name)
  "Show details of failed tests."
  (let ((suite (if (string= suite-name "ALL SUITES")
                   nil
                 (gethash (intern suite-name) kb-test-suites))))
    
    (if suite
        ;; Show failed tests for specific suite
        (dolist (test (kb-test-suite-tests suite))
          (when (eq (kb-test-status test) :failed)
            (message "FAILED: %s - %s" 
                    (kb-test-name test) 
                    (kb-test-error-message test))))
      ;; Show failed tests for all suites
      (maphash (lambda (name suite)
                 (dolist (test (kb-test-suite-tests suite))
                   (when (eq (kb-test-status test) :failed)
                     (message "FAILED: %s::%s - %s" 
                             name (kb-test-name test) 
                             (kb-test-error-message test)))))
               kb-test-suites))))

(defun kb-list-test-suite-names ()
  "Get list of all test suite names."
  (let ((names nil))
    (maphash (lambda (name suite) (push (symbol-name name) names)) kb-test-suites)
    (sort names #'string<)))

;;; Example Test Suite

(defun kb-setup-example-tests ()
  "Set up example test suites."
  
  (kb-deftest-suite basic-reasoning
    "Test basic knowledge base reasoning"
    
    (kb-deftest socrates-is-mortal
      "Test basic syllogistic reasoning"
      (kb-assert 'Socrates 'is-a 'human)
      (kb-assert 'human 'is-a 'mammal)  
      (kb-assert 'mammal 'is-mortal t)
      
      ;; Add mortality rule
      (kb-add-rule 'mortality-rule
                   '(((?x is-a ?y) (?y is-mortal t)))
                   '(?x is-mortal t))
      
      ;; Perform inference
      (kb-reason)
      
      ;; Test assertions
      (kb-assert-fact-exists 'Socrates 'is-mortal t))
    
    (kb-deftest bird-flying-default
      "Test default reasoning with exceptions"
      (kb-add-default 'birds-fly 
                      '((?x is-a bird)) 
                      '(?x can-fly t))
      
      (kb-add-exception 'penguin-exception 
                        'birds-fly
                        '((?x is-a penguin))
                        '(?x can-fly nil))
      
      (kb-assert 'Tweety 'is-a 'bird)
      (kb-assert 'Pingu 'is-a 'penguin)
      (kb-assert 'penguin 'is-a 'bird)
      
      (kb-reason)
      
      ;; Tweety should fly, Pingu should not
      (kb-assert-fact-exists 'Tweety 'can-fly t)
      (kb-assert-fact-exists 'Pingu 'can-fly nil)))
  
  (kb-deftest-suite microtheory-tests
    "Test microtheory functionality"
    
    (kb-deftest microtheory-inheritance
      "Test that child microtheories inherit from parents"
      (kb-with-test-microtheory parent-mt
        (kb-assert 'parent-fact 'test-prop 'parent-value)
        
        (let ((child-mt (kb-create-microtheory 'child-mt parent-mt)))
          (kb-with-microtheory child-mt
            ;; Should be able to query parent facts from child
            (kb-assert-fact-exists 'parent-fact 'test-prop 'parent-value)))))
    
    (kb-deftest microtheory-isolation
      "Test that microtheories provide isolation"
      (kb-with-test-microtheory mt1
        (kb-assert 'shared-entity 'property 'value1))
      
      (kb-with-test-microtheory mt2
        (kb-assert 'shared-entity 'property 'value2)
        ;; Should not see value1 from mt1
        (kb-assert-fact-not-exists 'shared-entity 'property 'value1)))))

;;; Interactive Commands

;;;###autoload
(defun kb-test-status ()
  "Show testing system status."
  (interactive)
  (message "Test suites: %d, Temp microtheories: %d"
           (hash-table-count kb-test-suites)
           (length kb-test-temp-microtheories)))

(provide 'kb-testing)
;;; kb-testing.el ends here
