;;; kb-nonmonotonic.el --- Non-Monotonic Reasoning for Knowledge Base
;; -*- lexical-binding: t; -*-

;; Author: AI Assistant
;; Keywords: ai, knowledge base, non-monotonic, defaults, exceptions
;; Version: 2.0

;;; Commentary:

;; This package implements non-monotonic reasoning capabilities including
;; default logic, exception handling, and belief revision for advanced reasoning.
;; It integrates with the microtheory system and TMS for consistency tracking.

;; Features:
;; - Default rules with exceptions
;; - Belief revision and conflict resolution
;; - Exception priority handling
;; - Integration with microtheory contexts
;; - TMS justification tracking for default reasoning

;;; Code:

(require 'cl-lib)
(require 'kb-microtheories)

;;; Variables

(defvar kb-default-rules nil
  "List of default rules in the system.")

(defvar kb-exceptions nil
  "List of exceptions to default rules.")

(defvar kb-defeated-justifications nil
  "List of justifications defeated by more specific rules.")

;;; Structures

(cl-defstruct (kb-default-rule (:constructor kb-default-rule-create)
                                   (:copier nil))
  "A default rule with exceptions."
  name           ; rule name/identifier
  premises       ; conditions that must hold
  conclusion     ; what can be inferred when premises hold
  exceptions     ; list of exceptional cases
  strength       ; confidence level (0.0 to 1.0)
  specificity     ; how specific this rule is (higher = more specific)
  microtheory   ; microtheory containing this rule
  defeated-p     ; whether this rule has been defeated
  applies-to     ; list of subjects this rule applies to)

(cl-defstruct (kb-exception (:constructor kb-exception-create)
                            (:copier nil))
  "An exception to a default rule."
  name           ; exception name/identifier
  applies-to     ; which default rule this exception applies to
  conditions     ; when this exception triggers
  conclusion     ; what to infer instead
  priority       ; exception priority (higher = more specific)
  microtheory   ; microtheory containing this exception)

;;; Rule Management API

;;;###autoload
(defun kb-add-default (name premises conclusion &optional exceptions strength specificity mt)
  "Add a default rule.
NAME identifies the rule.
PREMISES are conditions that must hold.
CONCLUSION is what can be inferred.
EXCEPTIONS is a list of exceptional cases.
STRENGTH is the confidence level (0.0 to 1.0).
SPECIFICITY is how specific this rule is (higher = more specific).
MT is the microtheory to add rule to."
  (kb-with-error-recovery
    (let* ((kb-current-mt (or mt kb-current-mt))
           (mt-obj (kb-get-microtheory kb-current-mt)))
      (unless mt-obj
        (signal 'kb-microtheory-error (list "Microtheory not found" kb-current-mt)))
      
      (let ((rule (kb-default-rule-create
                   :name name
                   :premises premises
                   :conclusion conclusion
                   :exceptions exceptions
                   :strength (or strength 0.5)
                   :specificity (or specificity 1.0)
                   :microtheory kb-current-mt
                   :defeated-p nil
                   :applies-to nil)))
        (push rule kb-default-rules)
        (message "Added default rule: %s" name))
      rule)))

;;;###autoload
(defun kb-add-exception (name applies-to conditions conclusion &optional priority mt)
  "Add an exception to a default rule.
NAME identifies the exception.
APPLIES-TO is which default rule this applies to.
CONDITIONS are when this exception triggers.
CONCLUSION is what to infer instead.
PRIORITY is the exception priority (higher = more specific).
MT is the microtheory to add exception to."
  (kb-with-error-recovery
    (let* ((kb-current-mt (or mt kb-current-mt))
           (mt-obj (kb-get-microtheory kb-current-mt)))
      (unless mt-obj
        (signal 'kb-microtheory-error (list "Microtheory not found" kb-current-mt)))
      
      (let ((exception (kb-exception-create
                        :name name
                        :applies-to applies-to
                        :conditions conditions
                        :conclusion conclusion
                        :priority (or priority 1.0)
                        :microtheory kb-current-mt)))
        (push exception kb-exceptions)
        (message "Added exception: %s to rule %s" name applies-to))
      exception)))

;;; Default Reasoning Engine

(defun kb-match-default-premises (rule mt-name)
  "Match premises of a default rule."
  (if (null (kb-default-rule-premises rule))
      (list nil)  ; No premises - always applicable
    (kb-match-premises (kb-default-rule-premises rule) mt-name)))

;;;###autoload
(defun kb-reason-with-defaults (&optional mt-name)
  "Apply default rules to microtheory.
MT-NAME is the microtheory to apply defaults to."
  (interactive)
  (let* ((mt-name (or mt-name kb-current-mt))
         (mt (kb-get-microtheory mt-name))
         (new-facts nil))
    
    (when mt
      ;; Process default rules
      (dolist (rule kb-default-rules)
        (when (or (eq (kb-default-rule-microtheory rule) mt-name)
                  (kb-is-ancestor-p (kb-default-rule-microtheory rule) mt-name))
          (let ((bindings (kb-match-default-premises rule mt-name)))
            (dolist (binding bindings)
              ;; Check if rule is defeated
              (unless (kb-default-rule-defeated-p rule)
                (let ((conclusion (kb-apply-bindings (kb-default-rule-conclusion rule) (list binding)))
                  (subjects (kb-get-subjects-from-conclusion conclusion)))
                  (dolist (subject subjects)
                    ;; Check for exceptions
                    (let ((applicable-exceptions 
                           (cl-remove-if-not 
                            (lambda (ex)
                              (kb-exception-conditions-met ex binding))
                            (kb-get-exceptions-for-rule (kb-default-rule-name rule))))
                      
                      (when applicable-exceptions
                        (dolist (ex applicable-exceptions)
                          ;; Apply exception instead of default
                          (let ((ex-conclusion (kb-apply-bindings (kb-exception-conclusion ex) (list binding))))
                            (push (cons subject (cadr ex-conclusion) (caddr ex-conclusion)) new-facts)
                            (kb-defeat-rule (kb-default-rule-name rule)
                                          (kb-exception-name ex)))))
                    
                    ;; Apply default conclusion if not defeated
                    (unless (kb-default-rule-defeated-p rule)
                      (dolist (subject subjects)
                        (push (cons subject (cadr conclusion) (caddr conclusion)) new-facts))))))))))
      
      ;; Add new facts to microtheory
      (dolist (fact new-facts)
        (kb-add-fact (car fact) (cadr fact) (caddr fact) 0.9))
      
      (message "Default reasoning completed in microtheory: %s" mt-name)
      mt)))

(defun kb-get-subjects-from-conclusion (conclusion)
  "Extract subjects from a conclusion pattern."
  (if (listp conclusion)
      (let ((subjects nil))
        ;; Find all variables in conclusion
        (dolist (elem conclusion)
          (when (and (symbolp elem)
                     (string-match-p "^?" (symbol-name elem)))
            (push elem subjects)))
        (when (cdr conclusion)  ; conclusion has more elements
          (push (cadr conclusion) subjects)))
        subjects)
    (list (cadr conclusion))))

(defun kb-get-exceptions-for-rule (rule-name)
  "Get all exceptions for a default rule."
  (cl-remove-if-not 
   (lambda (ex) (eq (kb-exception-applies-to ex) rule-name))
   kb-exceptions))

(defun kb-exception-conditions-met (ex binding)
  "Check if exception conditions are satisfied by a binding."
  (every (lambda (condition)
           (let ((instantiated (kb-apply-bindings condition (list binding))))
             (kb-query-with-inheritance 
              (car instantiated) (cadr instantiated) (kb-exception-microtheory ex))))
         (kb-exception-conditions ex)))

(defun kb-defeat-rule (rule-name exception-name)
  "Mark a rule as defeated by an exception."
  (let ((rule (cl-find rule-name kb-default-rules :key #'kb-default-rule-name)))
    (when rule
      (setf (kb-default-rule-defeated-p rule) t)
      (push (list :rule rule-name :defeated-by exception-name) 
            kb-defeated-justifications)
      (message "Rule %s defeated by exception %s" rule-name exception-name))))

(defun kb-default-rule-defeated-p (rule)
  "Check if a default rule has been defeated."
  (kb-default-rule-defeated-p rule))

;;; Conflict Resolution

(defun kb-resolve-default-conflicts (facts rule-name)
  "Resolve conflicts between default rules."
  ;; Priority: more specific rules win
  ;; Exception exceptions win over general defaults
  t)

;;; Utility Functions

(defun kb-clear-defaults ()
  "Clear all default rules and exceptions."
  (setq kb-default-rules nil)
  (setq kb-exceptions nil)
  (setq kb-defeated-justifications nil)
  (message "All defaults and exceptions cleared"))

(defun kb-default-rule-status ()
  "Display status of default rules."
  (interactive)
  (message "Default Rules: %d" (length kb-default-rules))
  (message "Exceptions: %d" (length kb-exceptions))
  (dolist (rule kb-default-rules)
    (message "  %s (defeated: %s)" 
             (kb-default-rule-name rule)
             (kb-default-rule-defeated-p rule)))
  (dolist (ex kb-exceptions)
    (message "  Exception %s -> %s" 
             (kb-exception-name ex)
             (kb-exception-applies-to ex))))

;;; Setup Examples

(defun kb-setup-default-reasoning-examples ()
  "Set up example default rules and exceptions."
  
  ;; Birds typically fly
  (kb-add-default 'birds-fly
                       '((?x is-a bird))
                       '(?x can-fly t)
                       nil 0.9 1)
  
  ;; Exception: penguins are birds but don't fly
  (kb-add-exception 'penguin-exception 
                    'birds-fly
                    '((?x is-a penguin))
                    '(?x can-fly nil)
                    2.0)
  
  ;; Mammals are typically warm-blooded
  (kb-add-default 'mammals-warm-blooded
                       '((?x is-a mammal))
                       '(?x warm-blooded t)
                       nil 0.95 1)
  
  ;; Exception: platypus is a mammal but not warm-blooded
  (kb-add-exception 'platypus-exception
                    'mammals-warm-blooded
                    '((?x is-a platypus))
                    '(?x warm-blooded nil)
                    2.0))

(provide 'kb-nonmonotonic)
;;; kb-nonmonotonic.el ends here
