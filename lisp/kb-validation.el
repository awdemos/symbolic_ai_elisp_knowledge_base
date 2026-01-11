;;; kb-validation.el --- Robust Error Handling and Validation for Knowledge Base System
;; -*- lexical-binding: t; -*-

;; Author: AI Assistant
;; Keywords: ai, knowledge base, validation, error handling
;; Version: 1.0

;;; Commentary:

;; This package provides comprehensive input validation, error handling,
;; and recovery mechanisms for the Knowledge Base system. It includes:
;; - Input validation for all major functions
;; - Microtheory validation and format checking
;; - Fact and rule validation with type checking
;; - Event validation for temporal facts
;; - Error recovery mechanisms with graceful degradation
;; - User-friendly error messages with suggestions
;; - Condition handling to prevent crashes

;;; Code:

(require 'cl-lib)

;;; Custom Error Types

(define-error 'kb-validation-error "Knowledge Base validation error")
(define-error 'kb-microtheory-error "Microtheory validation error" 'kb-validation-error)
(define-error 'kb-fact-error "Fact validation error" 'kb-validation-error)
(define-error 'kb-rule-error "Rule validation error" 'kb-validation-error)
(define-error 'kb-event-error "Event validation error" 'kb-validation-error)
(define-error 'kb-temporal-error "Temporal validation error" 'kb-validation-error)
(define-error 'kb-type-error "Type validation error" 'kb-validation-error)

;;; Validation Configuration

(defvar kb-validation-enabled t
  "Whether validation is enabled (can be disabled for performance).")

(defvar kb-validation-strict-mode nil
  "Whether to use strict validation (more restrictive checks).")

(defvar kb-validation-max-recursion-depth 100
  "Maximum recursion depth for validation checks.")

(defvar kb-validation-current-depth 0
  "Current recursion depth during validation.")

;;; Core Validation Predicates

(defun kb-validate-symbol (value &optional allow-nil)
  "Validate VALUE is a symbol.
If ALLOW-NIL is non-nil, nil is considered valid."
  (or (and allow-nil (null value))
      (symbolp value)
      (signal 'kb-type-error (list "Expected symbol" value))))

(defun kb-validate-string (value &optional allow-nil)
  "Validate VALUE is a string.
If ALLOW-NIL is non-nil, nil is considered valid."
  (or (and allow-nil (null value))
      (stringp value)
      (signal 'kb-type-error (list "Expected string" value))))

(defun kb-validate-number (value &optional allow-nil min-val max-val)
  "Validate VALUE is a number within optional bounds.
If ALLOW-NIL is non-nil, nil is considered valid."
  (when (or (not allow-nil) value)
    (unless (numberp value)
      (signal 'kb-type-error (list "Expected number" value)))
    (when (and min-val (< value min-val))
      (signal 'kb-type-error (list (format "Number below minimum %s" min-val) value)))
    (when (and max-val (> value max-val))
      (signal 'kb-type-error (list (format "Number above maximum %s" max-val) value))))
  t)

(defun kb-validate-list (value &optional allow-nil min-length max-length)
  "Validate VALUE is a list with optional length constraints.
If ALLOW-NIL is non-nil, nil is considered valid."
  (when (or (not allow-nil) value)
    (unless (listp value)
      (signal 'kb-type-error (list "Expected list" value)))
    (let ((len (length value)))
      (when (and min-length (< len min-length))
        (signal 'kb-type-error (list (format "List too short (minimum %s)" min-length) value)))
      (when (and max-length (> len max-length))
        (signal 'kb-type-error (list (format "List too long (maximum %s)" max-length) value)))))
  t)

;;; Microtheory Validation

(defun kb-validate-microtheory-name (name)
  "Validate microtheory NAME is properly formatted."
  (unless name
    (signal 'kb-microtheory-error '("Microtheory name cannot be nil")))
  
  (unless (symbolp name)
    (signal 'kb-microtheory-error (list "Microtheory name must be a symbol" name)))
  
  (let ((name-str (symbol-name name)))
    (when (string-match "^[0-9]" name-str)
      (signal 'kb-microtheory-error (list "Microtheory name cannot start with digit" name)))
    
    (when (string-match "[^a-zA-Z0-9_-]" name-str)
      (signal 'kb-microtheory-error (list "Microtheory name contains invalid characters" name)))
    
    (when (> (length name-str) 100)
      (signal 'kb-microtheory-error (list "Microtheory name too long (max 100 characters)" name))))
  
  t)

(defun kb-validate-microtheory-exists (name &optional should-exist)
  "Validate microtheory NAME exists or doesn't exist.
If SHOULD-EXIST is non-nil, validates it exists.
If SHOULD-EXIST is nil, validates it doesn't exist."
  (let ((exists-p (and (boundp 'kb-microtheories)
                       (gethash name kb-microtheories))))
    (if should-exist
        (unless exists-p
          (signal 'kb-microtheory-error (list "Microtheory does not exist" name)))
      (when exists-p
        (signal 'kb-microtheory-error (list "Microtheory already exists" name)))))
  t)

(defun kb-validate-microtheory-parents (parent-mts)
  "Validate PARENT-MTS is a valid list of parent microtheories."
  (when parent-mts
    (kb-validate-list parent-mts nil 1 100)
    (dolist (parent parent-mts)
      (kb-validate-microtheory-name parent)
      (kb-validate-microtheory-exists parent t))))
  t)

(defun kb-validate-inheritance-mode (mode)
  "Validate INHERITANCE-MODE is valid."
  (when mode
    (unless (memq mode '(strict override merge))
      (signal 'kb-microtheory-error 
              (list "Invalid inheritance mode (must be 'strict, 'override, or 'merge)" mode))))
  t)

(defun kb-validate-microtheory-priority (priority)
  "Validate microtheory PRIORITY is valid."
  (when priority
    (kb-validate-number priority nil 0 1000))
  t)

;;; Fact Validation

(defun kb-validate-fact-component (component component-name)
  "Validate a fact COMPONENT (subject, predicate, or object)."
  (unless component
    (signal 'kb-fact-error (list (format "%s cannot be nil" (capitalize component-name)))))
  
  ;; Allow symbols, strings, numbers, but not complex structures in strict mode
  (when kb-validation-strict-mode
    (unless (or (symbolp component) 
                (stringp component) 
                (numberp component)
                (and (listp component) (< (length component) 10))) ; reasonable list length
      (signal 'kb-fact-error 
              (list (format "%s has invalid type in strict mode" (capitalize component-name))
                    component))))
  t)

(defun kb-validate-fact-structure (subject predicate object)
  "Validate the basic structure of a fact with SUBJECT, PREDICATE, and OBJECT."
  (kb-validate-fact-component subject "subject")
  (kb-validate-fact-component predicate "predicate") 
  (kb-validate-fact-component object "object")
  
  ;; Additional semantic checks
  (when (eq subject object)
    (signal 'kb-fact-error (list "Subject and object cannot be identical" subject)))
  
  ;; Predicate should typically be a symbol
  (unless (symbolp predicate)
    (when kb-validation-strict-mode
      (signal 'kb-fact-error (list "Predicate should be a symbol" predicate))))
  
  t)

(defun kb-validate-certainty (certainty)
  "Validate CERTAINTY value is in valid range."
  (when certainty
    (kb-validate-number certainty nil 0.0 1.0))
  t)

(defun kb-validate-temporal-info (temporal-info)
  "Validate TEMPORAL-INFO structure."
  (when temporal-info
    (unless (or (listp temporal-info) 
                (and (boundp 'kb-temporal-info-p)
                     (funcall 'kb-temporal-info-p temporal-info)))
      (signal 'kb-temporal-error (list "Invalid temporal info structure" temporal-info))))
  t)

;;; Rule Validation

(defun kb-validate-rule-name (name)
  "Validate rule NAME is properly formatted."
  (unless name
    (signal 'kb-rule-error '("Rule name cannot be nil")))
  
  (unless (symbolp name)
    (signal 'kb-rule-error (list "Rule name must be a symbol" name)))
  t)

(defun kb-validate-rule-premises-structure (premises)
  "Validate rule PREMISES structure."
  (unless premises
    (signal 'kb-rule-error '("Rule premises cannot be nil")))
  
  (kb-validate-list premises nil 1 100) ; at least 1 premise, max 100
  
  ;; Each premise should be a valid fact or pattern
  (dolist (premise premises)
    (unless (listp premise)
      (signal 'kb-rule-error (list "Rule premise must be a list" premise)))
    
    (when (< (length premise) 2)
      (signal 'kb-rule-error (list "Rule premise too short (needs at least predicate and subject)" premise))))
  
  t)

(defun kb-validate-rule-conclusion (conclusion)
  "Validate rule CONCLUSION structure."
  (unless conclusion
    (signal 'kb-rule-error '("Rule conclusion cannot be nil")))
  
  (unless (listp conclusion)
    (signal 'kb-rule-error (list "Rule conclusion must be a list" conclusion)))
  
  (when (< (length conclusion) 2)
    (signal 'kb-rule-error (list "Rule conclusion too short" conclusion)))
  t)

(defun kb-validate-rule-priority (priority)
  "Validate rule PRIORITY."
  (when priority
    (kb-validate-number priority nil 0.0 1000.0))
  t)

(defun kb-validate-rule-temporal-p (temporal-p)
  "Validate rule TEMPORAL-P."
  (when (and temporal-p (not (null temporal-p)))
    (unless (booleanp temporal-p)
      (signal 'kb-type-error (list "Rule temporal-p must be boolean or nil" temporal-p)))
    t)
  t)

(defun kb-validate-rule-params (name premises conclusion &optional priority temporal-p mt)
  "Validate parameters for creating a rule."
  (kb-validate-rule-name name)
  (kb-validate-rule-premises-structure premises)
  (kb-validate-rule-conclusion conclusion)
  (kb-validate-rule-priority priority)
  (kb-validate-rule-temporal-p temporal-p)
  (when mt
    (kb-validate-microtheory-name mt)
    (kb-validate-microtheory-exists mt t))
  t)

(defun kb-validate-create-microtheory-params (name &optional parent-mts priority inheritance-mode)
  "Validate parameters for kb-create-microtheory function."
  (kb-validate-microtheory-name name)
  (kb-validate-microtheory-parents parent-mts)
  (kb-validate-microtheory-priority priority)
  (kb-validate-inheritance-mode inheritance-mode)
  t)

;;; Event Validation

(defun kb-validate-event-type (event-type)
  "Validate event EVENT-TYPE."
  (kb-validate-symbol event-type nil)
  t)

(defun kb-validate-event-participants (participants)
  "Validate event PARTICIPANTS."
  (kb-validate-list participants nil 1 100)
  t)

(defun kb-validate-event-time (time time-type)
  "Validate event TIME based on TIME-TYPE (:start, :end, :duration)."
  (when (and time (not (or (stringp time) (numberp time) (listp time)))
    (signal 'kb-type-error (list "Event time must be string, number, or list" time)))
  (when (and (eq time-type :duration) (or (listp time) (numberp time))
    (signal 'kb-type-error (list "Duration must be a positive number" time)))
  t)

(defun kb-validate-event-duration (duration)
  "Validate event DURATION."
  (when (and duration (or (not (numberp duration)) (< duration 0)))
    (signal 'kb-type-error (list "Duration must be a positive number" duration))
  t)

(defun kb-validate-event-params (event-type &rest properties)
  "Validate event parameters."
  (kb-validate-event-type event-type)
  (dolist (prop properties)
    (let ((prop-type (car prop))
          (prop-value (cdr prop)))
      (cond
       ((eq prop-type :participants)
        (kb-validate-event-participants prop-value))
       ((eq prop-type :location)
        (kb-validate-symbol prop-value t))
       ((eq prop-type :start-time)
        (kb-validate-event-time prop-value :start))
       ((eq prop-type :end-time)
        (kb-validate-event-time prop-value :end))
       ((eq prop-type :duration)
        (kb-validate-event-duration prop-value))))))
  t)

;;; Function Parameter Validation

(defun kb-validate-assert-params (subject predicate object &optional certainty temporal-info mt)
  "Validate parameters for kb-assert function."
  (kb-validate-fact-structure subject predicate object)
  (kb-validate-certainty certainty)
  (kb-validate-temporal-info temporal-info)
  (when mt
    (kb-validate-microtheory-name mt)
    (kb-validate-microtheory-exists mt t))
  t)

(defun kb-validate-query-params (subject predicate &optional mt)
  "Validate parameters for kb-query function."
  (kb-validate-symbol subject t)
  (kb-validate-symbol predicate t)
  (when mt
    (kb-validate-microtheory-name mt)
    (kb-validate-microtheory-exists mt t))
  t)

(defun kb-validate-retract-params (subject predicate object &optional mt)
  "Validate parameters for kb-retract function."
  (kb-validate-fact-structure subject predicate object)
  (when mt
    (kb-validate-microtheory-name mt)
    (kb-validate-microtheory-exists mt t))
  t)

;;; Macro and Helper Functions

(defmacro kb-with-validation (function-name params &rest body)
  "Execute BODY with validation enabled.
FUNCTION-NAME is the name of the function being validated.
PARAMS are the parameters to validate.
BODY is the code to execute if validation passes."
  `(let ((validation-result
          (when kb-validation-enabled
            (condition-case err
                (progn ,@body)
              (kb-validation-error 
               (kb-validation-error-handler (car err) (cdr err)))))))
     (or validation-result
         (signal 'kb-validation-error
                 (list "Validation failed for function" ',function-name)))))

(defun kb-validation-error-handler (error-type error-data)
  "Handle validation errors with user-friendly messages and suggestions."
  (let ((error-msg (car error-data))
         (error-value (cadr error-data))
         (suggestion nil))
    
    ;; Generate suggestions based on error type
    (setq suggestion
          (cond
           ((string-match "not exist" error-msg)
            (format "Try creating the %s first with (kb-create-microtheory %s)"
                    (if (string-match "microtheory" error-msg) "microtheory" "item")))
           ((string-match "cannot be nil" error-msg)
            "Provide a valid value")
           ((string-match "invalid" error-msg)
            "Check the documentation for valid parameter formats")
           (t
            nil)))
    
    ;; Print error message with suggestion
    (message "KB Validation Error: %s" error-msg)
    (message "Suggestion: %s" suggestion)
    
    ;; Return a clean error
    (signal 'kb-validation-error (list error-msg error-value suggestion))))

(defmacro kb-with-error-recovery (&rest body)
  "Execute BODY with error recovery.
If an error occurs, log it and return a sensible default."
  `(condition-case err
       (progn ,@body)
     (error 
      (message "KB System Error: %s\nOperation failed gracefully." (error-message-string err))
      nil)))

;;; System Validation

(defun kb-validate-system ()
  "Validate consistency of the entire KB system."
  (let ((errors nil)
        (validation-depth 0))
    
    ;; Check microtheory hierarchy
    (when (boundp 'kb-microtheories)
      (maphash (lambda (name mt)
                 (let ((validation-depth (+ validation-depth 1)))
                   (when (> validation-depth kb-validation-max-recursion-depth)
                     (signal 'kb-validation-error 
                             (list "Maximum recursion depth exceeded")))
                   (kb-validate-microtheory-name name)
                   (kb-validate-microtheory-parents (kb-microtheory-parent-mts mt))))
               kb-microtheories)))
    
    ;; Check for circular inheritance
    (when (boundp 'kb-microtheories)
      (maphash (lambda (name mt)
                 (kb-validate-inheritance-cycle name (kb-microtheory-parent-mts mt)))
               kb-microtheories))
    
    (if errors
        (progn
          (message "KB System validation found %d errors:" (length errors))
          (dolist (error errors)
            (message "  %s" error)))
          nil)
      (message "KB System validation passed")
      t)))

;;; User-Facing Validation Functions

;;;###autoload
(defun kb-check-fact (subject predicate object)
  "Check if a fact is valid without asserting it."
Returns t if valid, signals error otherwise."
  (interactive "sCheck fact (s p o): ")
  (kb-validate-fact-structure subject predicate object)
  (message "Fact is valid: (%s %s %s)" subject predicate object))

;;;###autoload
(defun kb-check-rule (name premises conclusion)
  "Check if a rule is valid without adding it.
Returns t if valid, signals error otherwise."
  (interactive "sCheck rule\nName: \sPremises: \nConclusion: ")
  (kb-validate-rule-params name premises conclusion)
  (message "Rule is valid: %s" name))

;;;###autoload
(defun kb-check-microtheory (name parent-mts)
  "Check if a microtheory is valid without creating it.
Returns t if valid, signals error otherwise."
  (interactive "sCheck microtheory\nName: \sParents: ")
  (kb-validate-create-microtheory-params name parent-mts)
  (message "Microtheory is valid: %s" name))

;;; Validation Control

;;;###autoload
(defun kb-validation-toggle ()
  "Toggle validation on/off."
  (interactive)
  (setq kb-validation-enabled (not kb-validation-enabled))
  (message "Validation %s" (if kb-validation-enabled "enabled" "disabled")))

;;;###autoload
(defun kb-validation-toggle-strict-mode ()
  "Toggle strict validation mode."
  (interactive)
  (setq kb-validation-strict-mode (not kb-validation-strict-mode))
  (message "Strict mode %s" (if kb-validation-strict-mode "enabled" "disabled")))

;;;###autoload
(defun kb-validation-show-stats ()
  "Show validation statistics."
  (interactive)
  (message "KB Validation Statistics:")
  (message "  Validation enabled: %s" kb-validation-enabled)
  (message "  Strict mode: %s" kb-validation-strict-mode)
  (message "  Max recursion depth: %d" kb-validation-max-recursion-depth)
  (message "  Current depth: %d" kb-validation-current-depth))

(provide 'kb-validation)
;;; kb-validation.el ends here
