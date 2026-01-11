;;; test-inheritance.el --- Test enhanced microtheory inheritance system

(require 'kb-tms)
(require 'kb-microtheories)

(defun test-microtheory-inheritance ()
  "Test the enhanced microtheory inheritance system."
  (interactive)
  
  ;; Test 1: Basic inheritance
  (message "=== Test 1: Basic Inheritance ===")
  (kb-with-microtheory 'BaseMt
    (kb-add-fact 'socrates 'isa 'human)
    (kb-add-fact 'human 'mortal 'yes))
  
  (kb-with-microtheory 'CommonSenseMt
    (kb-add-fact 'socrates 'wise 'yes)
    (kb-add-fact 'socrates 'mortal 'very-much))  ; shadows parent
  
  (let ((inherited-facts (kb-query-with-inheritance 'socrates 'isa 'CommonSenseMt))
        (shadowed-facts (kb-query-with-inheritance 'socrates 'mortal 'CommonSenseMt)))
    (message "Inherited fact (isa): %s" 
             (mapcar (lambda (f) (list (kb-fact-object f) (kb-fact-microtheory f))) inherited-facts))
    (message "Shadowed fact (mortal): %s"
             (mapcar (lambda (f) (list (kb-fact-object f) (kb-fact-microtheory f))) shadowed-facts)))
  
  ;; Test 2: Multiple inheritance with conflict resolution
  (message "\n=== Test 2: Multiple Inheritance ===")
  (kb-create-microtheory 'PhilosophyMt '(BaseMt) 2 'override)
  (kb-create-microtheory 'GreekMt '(BaseMt) 2 'merge)
  (kb-create-microtheory 'AncientPhilosopherMt (list 'PhilosophyMt 'GreekMt) 4 'merge)
  
  (kb-with-microtheory 'PhilosophyMt
    (kb-add-fact 'socrates 'profession 'philosopher)
    (kb-add-fact 'socrates 'famous 'very))
    
  (kb-with-microtheory 'GreekMt
    (kb-add-fact 'socrates 'nationality 'greek)
    (kb-add-fact 'socrates 'famous 'somewhat))  ; conflict with PhilosophyMt
    
  (kb-with-microtheory 'AncientPhilosopherMt
    (kb-add-fact 'socrates 'period 'ancient))
  
  (message "Multiple inheritance chain: %s" (kb-get-inheritance-chain 'AncientPhilosopherMt))
  (dolist (pred '(profession nationality famous period))
    (let ((facts (kb-query-with-inheritance 'socrates pred 'AncientPhilosopherMt)))
      (when facts
        (message "%s: %s" pred 
                 (mapcar (lambda (f) (list (kb-fact-object f) (kb-fact-microtheory f))) facts)))))
  
  ;; Test 3: Cycle detection
  (message "\n=== Test 3: Cycle Detection ===")
  (condition-case err
      (kb-add-parent-microtheory 'BaseMt 'AncientPhilosopherMt)
    (error (message "Cycle correctly prevented: %s" (error-message-string err))))
  
  ;; Test 4: Local facts (non-inheritable)
  (message "\n=== Test 4: Local Facts ===")
  (kb-with-microtheory 'BaseMt
    (kb-add-fact 'secret 'password '12345 1.0 nil t))  ; local fact
  
  (let ((inherited (kb-query-with-inheritance 'secret 'password 'CommonSenseMt))
        (local (kb-query-in-mt 'secret 'password 'BaseMt)))
    (message "Local fact in BaseMt: %s" (length local))
    (message "Inherited in CommonSenseMt: %s" (length inherited)))
  
  ;; Test 5: TMS integration
  (message "\n=== Test 5: TMS Integration ===")
  (kb-with-microtheory 'CommonSenseMt
    (kb-add-fact-with-justification 'plato 'student 'socrates 
                                   '((socrates isa philosopher)) 'teacher-student-rule))
  
  (let ((justified-facts (kb-query-with-tms-check 'plato 'student 'CommonSenseMt)))
    (message "TMS-verified facts: %s" 
             (mapcar (lambda (f) (list (kb-fact-object f) (kb-fact-microtheory f))) justified-facts)))
  
  ;; Test 6: System status and validation
  (message "\n=== Test 6: System Status ===")
  (message "System status: %s" (kb-microtheory-status))
  (message "TMS status: %s" (kb-tms-status))
  (message "System validation: %s" (kb-microtheory-validate))
  
  ;; Test 7: Microtheory relationships
  (message "\n=== Test 7: Microtheory Relationships ===")
  (message "CommonSenseMt ancestors: %s" (kb-get-ancestors 'CommonSenseMt))
  (message "BaseMt descendants: %s" (kb-get-descendants 'BaseMt))
  (message "Is BaseMt ancestor of CommonSenseMt? %s" (kb-is-ancestor-p 'BaseMt 'CommonSenseMt))
  
  (message "\n=== All tests completed ==="))

;; Run tests when loaded
(test-microtheory-inheritance)

(provide 'test-inheritance)
;;; test-inheritance.el ends here
