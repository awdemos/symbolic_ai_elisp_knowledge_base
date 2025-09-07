;;; validation-demo.el --- Demonstration of KB Validation System

;; This file demonstrates the robust error handling and validation
;; features added to the Knowledge Base system.

(add-to-list 'load-path (expand-file-name "../lisp" (file-name-directory load-file-name)))
(require 'kb-advanced-system)

;; Initialize the system
(kb-init)

(message "\n=== KB VALIDATION SYSTEM DEMONSTRATION ===\n")

;; 1. Demonstrate input validation for facts
(message "1. Testing fact validation...")

;; Valid fact - should succeed
(message "✓ Valid fact assertion:")
(kb-assert 'John 'loves 'Mary)
(message "   Successfully asserted: (John loves Mary)")

;; Invalid fact - should fail gracefully
(message "✗ Invalid fact assertion (nil subject):")
(condition-case err
    (kb-assert nil 'loves 'Mary)
  (error (message "   Caught error: %s" (error-message-string err))))

;; Invalid fact - should fail gracefully
(message "✗ Invalid fact assertion (subject equals object):")
(condition-case err
    (kb-assert 'John 'is 'John)
  (error (message "   Caught error: %s" (error-message-string err))))

;; 2. Demonstrate microtheory validation
(message "\n2. Testing microtheory validation...")

;; Valid microtheory creation
(message "✓ Valid microtheory creation:")
(kb-create-microtheory 'TestMt 'BaseMt)
(message "   Successfully created microtheory: TestMt")

;; Invalid microtheory - already exists
(message "✗ Invalid microtheory creation (already exists):")
(condition-case err
    (kb-create-microtheory 'TestMt 'BaseMt)
  (error (message "   Caught error: %s" (error-message-string err))))

;; Invalid microtheory name
(message "✗ Invalid microtheory creation (bad name):")
(condition-case err
    (kb-create-microtheory "123BadName" 'BaseMt)
  (error (message "   Caught error: %s" (error-message-string err))))

;; 3. Demonstrate query validation
(message "\n3. Testing query validation...")

;; Valid query
(message "✓ Valid query:")
(let ((results (kb-query 'John 'loves)))
  (message "   Query results: %s" results))

;; Query with invalid microtheory
(message "✗ Invalid query (non-existent microtheory):")
(condition-case err
    (kb-query 'John 'loves 'NonExistentMt)
  (error (message "   Caught error: %s" (error-message-string err))))

;; 4. Demonstrate rule validation
(message "\n4. Testing rule validation...")

;; Valid rule
(message "✓ Valid rule creation:")
(kb-add-rule 'love-symmetry
             '((loves ?x ?y))
             '(loves ?y ?x))
(message "   Successfully added rule: love-symmetry")

;; Invalid rule - empty premises
(message "✗ Invalid rule creation (empty premises):")
(condition-case err
    (kb-add-rule 'bad-rule nil '(conclusion))
  (error (message "   Caught error: %s" (error-message-string err))))

;; 5. Demonstrate event validation
(message "\n5. Testing event validation...")

;; Valid event
(message "✓ Valid event creation:")
(let ((event-id (kb-create-event 'Meeting
                                 :participants '(John Mary)
                                 :start-time "2024-01-01"
                                 :duration 60)))
  (message "   Successfully created event: %s" event-id))

;; Invalid event - bad duration
(message "✗ Invalid event creation (negative duration):")
(condition-case err
    (kb-create-event 'BadMeeting :duration -30)
  (error (message "   Caught error: %s" (error-message-string err))))

;; 6. Demonstrate validation toggles
(message "\n6. Testing validation controls...")

;; Show current validation status
(message "Current validation status:")
(message "   Validation enabled: %s" kb-validation-enabled)
(message "   Strict mode: %s" kb-validation-strict-mode)

;; Toggle strict mode
(kb-validation-toggle-strict-mode)
(message "   Toggled strict mode: %s" kb-validation-strict-mode)

;; Test with strict mode
(message "✗ Testing with strict mode (list as predicate):")
(condition-case err
    (kb-assert 'John '(complex predicate) 'Mary)
  (error (message "   Caught error in strict mode: %s" (error-message-string err))))

;; Toggle back
(kb-validation-toggle-strict-mode)

;; 7. Demonstrate system validation
(message "\n7. Testing system validation...")
(kb-validate-system)

;; 8. Demonstrate user validation functions
(message "\n8. Testing user validation functions...")

(message "✓ Checking valid fact:")
(kb-check-fact 'Alice 'knows 'Bob)

(message "✗ Checking invalid fact:")
(kb-check-fact nil 'knows 'Bob)

(message "✓ Checking valid microtheory name:")
(kb-check-microtheory 'ValidName)

(message "✗ Checking invalid microtheory name:")
(kb-check-microtheory '123Invalid)

;; 9. Show validation statistics
(message "\n9. Validation statistics:")
(kb-validation-show-stats)

(message "\n=== VALIDATION DEMO COMPLETE ===")
(message "The KB system now includes comprehensive validation and error handling!")

;;; validation-demo.el ends here
