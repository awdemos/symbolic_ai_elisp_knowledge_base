;;; kb-validation.el --- Robust Error Handling and Validation for Knowledge Base System

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
    (kb-validate-list parent-mts)
    (dolist (parent parent-mts)
      (kb-validate-microtheory-name parent)
      (kb-validate-microtheory-exists parent t)))
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
  
  (kb-validate-symbol name)
  
  (let ((name-str (symbol-name name)))
    (when (> (length name-str) 100)
      (signal 'kb-rule-error (list "Rule name too long (max 100 characters)" name))))
  
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
  "Validate rule PRIORITY value."
  (when priority
    (kb-validate-number priority nil 0.0 10.0))
  t)

;;; Event Validation

(defun kb-validate-event-type (event-type)
  "Validate EVENT-TYPE is valid."
  (kb-validate-symbol event-type)
  
  ;; Check against known event types if available
  (when (and (boundp 'kb-event-types) kb-event-types)
    (unless (member event-type kb-event-types)
      (signal 'kb-event-error 
              (list "Unknown event type (consider adding to kb-event-types)" event-type))))
  
  t)

(defun kb-validate-event-participants (participants)
  "Validate event PARTICIPANTS list."
  (when participants
    (kb-validate-list participants)
    (dolist (participant participants)
      (kb-validate-symbol participant)))
  t)

(defun kb-validate-event-time (time time-type)
  "Validate event TIME value of TIME-TYPE (start-time, end-time, etc.)."
  (when time
    ;; Accept various time formats
    (unless (or (numberp time)
                (stringp time)
                (and (listp time) (= (length time) 3))) ; (year month day)
      (signal 'kb-temporal-error 
              (list (format "Invalid %s format" time-type) time))))
  t)

(defun kb-validate-event-duration (duration)
  "Validate event DURATION."
  (when duration
    (unless (and (numberp duration) (> duration 0))
      (signal 'kb-temporal-error (list "Duration must be positive number" duration))))
  t)

;;; High-Level Validation Functions

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
  (when subject
    (kb-validate-fact-component subject "subject"))
  (when predicate  
    (kb-validate-fact-component predicate "predicate"))
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

(defun kb-validate-rule-params (name premises conclusion &optional priority temporal-p mt)
  "Validate parameters for creating a rule."
  (kb-validate-rule-name name)
  (kb-validate-rule-premises-structure premises)
  (kb-validate-rule-conclusion conclusion)
  (kb-validate-rule-priority priority)
  (when mt
    (kb-validate-microtheory-name mt)
    (kb-validate-microtheory-exists mt t))
  t)

(defun kb-validate-create-microtheory-params (name &optional parent-mts priority inheritance-mode)
  "Validate parameters for kb-create-microtheory function."
  (kb-validate-microtheory-name name)
  (kb-validate-microtheory-exists name nil) ; should not exist
  (kb-validate-microtheory-parents parent-mts)
  (kb-validate-microtheory-priority priority)
  (kb-validate-inheritance-mode inheritance-mode)
  t)

(defun kb-validate-event-params (event-type &rest properties)
  "Validate parameters for kb-create-event function."
  (kb-validate-event-type event-type)
  
  (let ((participants (plist-get properties :participants))
        (start-time (plist-get properties :start-time))
        (end-time (plist-get properties :end-time))
        (duration (plist-get properties :duration)))
    
    (kb-validate-event-participants participants)
    (kb-validate-event-time start-time "start-time")
    (kb-validate-event-time end-time "end-time")
    (kb-validate-event-duration duration)
    
    ;; Validate temporal consistency
    (when (and start-time end-time duration)
      (when kb-validation-strict-mode
        (signal 'kb-temporal-error 
                '("Cannot specify both end-time and duration with start-time")))))
  
  t)

;;; Error Recovery and Suggestions

(defun kb-suggest-correction (error-data)
  "Suggest corrections based on ERROR-DATA."
  (let ((error-type (car error-data))
        (error-value (cadr error-data)))
    (cond
     ((string-match "does not exist" error-type)
      (format "Try creating the microtheory first with (kb-create-microtheory %s)" error-value))
     
     ((string-match "already exists" error-type)
      (format "Try using a different name or use (kb-get-microtheory %s) to retrieve existing" error-value))
     
     ((string-match "cannot be nil" error-type)
      "Provide a valid value (non-nil)")
     
     ((string-match "must be a symbol" error-type)
      "Use a symbol like 'my-symbol instead of a string")
     
     ((string-match "Invalid.*mode" error-type)
      "Use one of: 'strict, 'override, or 'merge")
     
     (t "Check the documentation for valid parameter formats"))))

(defun kb-validation-error-handler (error-symbol error-data)
  "Handle validation errors with helpful messages."
  (let ((suggestion (kb-suggest-correction error-data)))
    (message "KB Validation Error: %s\nSuggestion: %s" 
             (error-message-string (list error-symbol error-data))
             suggestion)
    ;; Return nil to indicate error was handled
    nil))

;;; Validation Wrapper Macros

(defmacro kb-with-validation (func-name params &rest body)
  "Execute BODY with validation for FUNC-NAME using PARAMS."
  `(if kb-validation-enabled
       (condition-case err
           (progn
             ,(pcase func-name
                ('kb-assert `(apply #'kb-validate-assert-params ,params))
                ('kb-query `(apply #'kb-validate-query-params ,params))
                ('kb-retract `(apply #'kb-validate-retract-params ,params))
                ('kb-add-rule `(apply #'kb-validate-rule-params ,params))
                ('kb-create-microtheory `(apply #'kb-validate-create-microtheory-params ,params))
                ('kb-create-event `(apply #'kb-validate-event-params ,params)))
             ,@body)
         (kb-validation-error
          (kb-validation-error-handler (car err) (cdr err))))
     (progn ,@body)))

(defmacro kb-with-error-recovery (&rest body)
  "Execute BODY with comprehensive error recovery."
  `(condition-case err
       (progn ,@body)
     (kb-validation-error
      (kb-validation-error-handler (car err) (cdr err)))
     (error
      (message "KB System Error: %s\nOperation failed gracefully." 
               (error-message-string err))
      nil)))

;;; User-Accessible Validation Functions

(defun kb-validate-system ()
  "Validate the current state of the KB system."
  (interactive)
  (condition-case err
      (progn
        (message "Validating KB system...")
        
        ;; Check microtheories
        (when (boundp 'kb-microtheories)
          (maphash (lambda (name mt)
                     (kb-validate-microtheory-name name)
                     (when (kb-microtheory-parent-mts mt)
                       (kb-validate-microtheory-parents 
                        (kb-microtheory-parent-mts mt))))
                   kb-microtheories))
        
        ;; Check current microtheory
        (when (boundp 'kb-current-mt)
          (kb-validate-microtheory-name kb-current-mt)
          (kb-validate-microtheory-exists kb-current-mt t))
        
        (message "KB system validation completed successfully."))
    (error
     (message "KB system validation failed: %s" (error-message-string err)))))

(defun kb-check-fact (subject predicate object)
  "Check if a fact is valid without asserting it."
  (interactive "sSubject: \nsPredicate: \nsObject: ")
  (condition-case err
      (progn
        (kb-validate-fact-structure subject predicate object)
        (message "Fact (%s %s %s) is valid" subject predicate object)
        t)
    (kb-validation-error
     (message "Invalid fact: %s" (error-message-string err))
     nil)))

(defun kb-check-microtheory (name)
  "Check if a microtheory name is valid."
  (interactive "sMicrotheory name: ")
  (condition-case err
      (progn
        (kb-validate-microtheory-name name)
        (message "Microtheory name '%s' is valid" name)
        t)
    (kb-validation-error
     (message "Invalid microtheory name: %s" (error-message-string err))
     nil)))

(defun kb-validation-toggle-strict-mode ()
  "Toggle strict validation mode."
  (interactive)
  (setq kb-validation-strict-mode (not kb-validation-strict-mode))
  (message "KB validation strict mode: %s" 
           (if kb-validation-strict-mode "ENABLED" "DISABLED")))

(defun kb-validation-enable ()
  "Enable KB validation."
  (interactive)
  (setq kb-validation-enabled t)
  (message "KB validation enabled"))

(defun kb-validation-disable ()
  "Disable KB validation (for performance)."
  (interactive)
  (setq kb-validation-enabled nil)
  (message "KB validation disabled"))

;;; Statistics and Debugging

(defvar kb-validation-stats (make-hash-table :test 'eq)
  "Statistics on validation calls.")

(defun kb-validation-record-stat (func-name)
  "Record statistics for validation of FUNC-NAME."
  (let ((count (gethash func-name kb-validation-stats 0)))
    (puthash func-name (1+ count) kb-validation-stats)))

(defun kb-validation-show-stats ()
  "Show validation statistics."
  (interactive)
  (message "KB Validation Statistics:")
  (maphash (lambda (func count)
             (message "  %s: %d calls validated" func count))
           kb-validation-stats))

(provide 'kb-validation)

;;; kb-validation.el ends here
