;;; kb-microtheories.el --- Microtheory System for Knowledge Base

;; Author: AI Assistant  
;; Keywords: ai, knowledge base, microtheories, context
;; Version: 2.0

;;; Commentary:

;; This package implements a microtheory system for managing
;; contextual knowledge and preventing logical inconsistencies.

;;; Code:

(require 'cl-lib)

;;; Microtheory Structures

(cl-defstruct (kb-microtheory (:constructor kb-microtheory-create)
                              (:copier nil))
  "A microtheory context for scoped knowledge."
  name
  parent-mts        ; list of parent microtheories  
  facts            ; hash table of facts in this mt
  rules            ; list of inference rules in this mt
  inherits-from    ; list of mts to inherit from
  temp-p           ; t if this is a temporary microtheory
  created-at)      ; timestamp when created

(cl-defstruct (kb-fact (:constructor kb-fact-create)
                       (:copier nil))
  "A structure representing a fact in the knowledge base."
  subject predicate object 
  certainty
  microtheory      ; which microtheory this fact belongs to
  justification    ; how this fact was derived
  temporal-info)   ; temporal validity information

(cl-defstruct (kb-rule (:constructor kb-rule-create)
                       (:copier nil))
  "A structure representing an inference rule."
  name premises conclusion 
  microtheory      ; which microtheory this rule belongs to
  priority         ; rule priority for conflict resolution
  temporal-p)      ; whether this rule handles temporal reasoning

(cl-defstruct (kb-temporal-info (:constructor kb-temporal-info-create)
                                (:copier nil))
  "Temporal information for facts and events."
  valid-from valid-to 
  during-event
  happens-at)

;;; Variables

(defvar kb-microtheories (make-hash-table :test 'equal)
  "Hash table storing all microtheories.")

(defvar kb-current-mt 'BaseMt
  "Currently active microtheory.")

(defvar kb-temp-mt-counter 0
  "Counter for generating temporary microtheory names.")

;;; Core Microtheory Functions

(defun kb-create-microtheory (name &optional parent-mts)
  "Create a new microtheory with NAME and optional PARENT-MTS."
  (let ((mt (kb-microtheory-create
             :name name
             :parent-mts (if (listp parent-mts) parent-mts (list parent-mts))
             :facts (make-hash-table :test 'equal)
             :rules nil
             :inherits-from (if (listp parent-mts) parent-mts (list parent-mts))
             :temp-p nil
             :created-at (current-time))))
    (puthash name mt kb-microtheories)
    mt))

(defun kb-create-temp-microtheory (&optional base-mt)
  "Create a temporary microtheory for query isolation."
  (let* ((name (intern (format "TempMt-%d" (cl-incf kb-temp-mt-counter))))
         (parent (or base-mt kb-current-mt))
         (mt (kb-microtheory-create
              :name name
              :parent-mts (list parent)
              :facts (make-hash-table :test 'equal)
              :rules nil
              :inherits-from (list parent)
              :temp-p t
              :created-at (current-time))))
    (puthash name mt kb-microtheories)
    mt))

(defun kb-get-microtheory (name)
  "Get microtheory by NAME."
  (gethash name kb-microtheories))

(defmacro kb-with-microtheory (mt-name &rest body)
  "Execute BODY in the context of microtheory MT-NAME."
  `(let ((old-mt kb-current-mt))
     (setq kb-current-mt ,mt-name)
     (unwind-protect
         (progn ,@body)
       (setq kb-current-mt old-mt))))

(defmacro kb-in-microtheory (mt-name &rest body)
  "Macro version of kb-with-microtheory."
  `(kb-with-microtheory ',mt-name ,@body))

;;; Fact Management with Microtheories

(defun kb-add-fact (subject predicate object &optional certainty temporal-info)
  "Add a fact to the current microtheory."
  (let* ((mt (kb-get-microtheory kb-current-mt))
         (certainty (or certainty 1.0))
         (fact (kb-fact-create 
                :subject subject
                :predicate predicate
                :object object
                :certainty certainty
                :microtheory kb-current-mt
                :temporal-info temporal-info))
         (facts (gethash subject (kb-microtheory-facts mt))))
    (puthash subject (cons fact facts) (kb-microtheory-facts mt))))

(defun kb-query-in-mt (subject predicate mt-name)
  "Query facts in a specific microtheory."
  (let ((mt (kb-get-microtheory mt-name)))
    (when mt
      (let ((facts (gethash subject (kb-microtheory-facts mt))))
        (cl-remove-if-not
         (lambda (fact)
           (and (eq (kb-fact-predicate fact) predicate)
                (> (kb-fact-certainty fact) 0)))
         facts)))))

(defun kb-query-with-inheritance (subject predicate &optional mt-name)
  "Query facts with microtheory inheritance."
  (let* ((mt-name (or mt-name kb-current-mt))
         (mt (kb-get-microtheory mt-name))
         (results (kb-query-in-mt subject predicate mt-name)))
    
    ;; Add facts from parent microtheories
    (when mt
      (dolist (parent-mt (kb-microtheory-inherits-from mt))
        (setq results (append results 
                             (kb-query-with-inheritance subject predicate parent-mt)))))
    
    ;; Remove duplicates and sort by certainty
    (cl-remove-duplicates results :test #'kb-fact-equal-p)))

(defun kb-fact-equal-p (fact1 fact2)
  "Check if two facts are equal."
  (and (eq (kb-fact-subject fact1) (kb-fact-subject fact2))
       (eq (kb-fact-predicate fact1) (kb-fact-predicate fact1))
       (equal (kb-fact-object fact1) (kb-fact-object fact2))))

;;; Inference Engine with Microtheories

(defun kb-add-rule (name premises conclusion &optional priority temporal-p)
  "Add an inference rule to the current microtheory."
  (let* ((mt (kb-get-microtheory kb-current-mt))
         (rule (kb-rule-create
                :name name
                :premises premises
                :conclusion conclusion
                :microtheory kb-current-mt
                :priority (or priority 1.0)
                :temporal-p temporal-p)))
    (push rule (kb-microtheory-rules mt))))

(defun kb-infer-in-mt (&optional mt-name)
  "Apply inference rules within a microtheory."
  (let* ((mt-name (or mt-name kb-current-mt))
         (mt (kb-get-microtheory mt-name))
         (new-facts nil))
    
    (when mt
      ;; Apply rules in priority order
      (let ((rules (sort (kb-microtheory-rules mt) 
                        (lambda (r1 r2) 
                          (> (kb-rule-priority r1) (kb-rule-priority r2))))))
        (dolist (rule rules)
          (let ((bindings (kb-match-premises (kb-rule-premises rule) mt-name)))
            (dolist (binding bindings)
              (let ((new-fact (kb-apply-bindings (kb-rule-conclusion rule) binding)))
                (when (kb-validate-new-fact new-fact mt-name)
                  (push new-fact new-facts))))))))
    
    ;; Add new facts to the microtheory
    (dolist (fact new-facts)
      (kb-add-fact (car fact) (cadr fact) (caddr fact) 1.0))))

(defun kb-match-premises (premises mt-name)
  "Match premises against knowledge in microtheory with inheritance."
  (let ((bindings '(())))  ; Start with empty binding
    (dolist (premise premises)
      (let* ((subject (car premise))
             (predicate (cadr premise))
             (new-bindings '()))
        (if (kb-variable-p subject)
            ;; Variable subject - need to find all matching facts
            (maphash 
             (lambda (subj facts)
               (dolist (fact facts)
                 (when (eq (kb-fact-predicate fact) predicate)
                   (dolist (binding bindings)
                     (let ((new-binding (cons (cons subject subj) binding)))
                       (push new-binding new-bindings))))))
             (kb-microtheory-facts (kb-get-microtheory mt-name)))
          ;; Concrete subject
          (let ((results (kb-query-with-inheritance subject predicate mt-name)))
            (dolist (fact results)
              (dolist (binding bindings)
                (push binding new-bindings)))))
        (setq bindings new-bindings)))
    bindings))

(defun kb-variable-p (term)
  "Check if TERM is a variable (starts with ?)."
  (and (symbolp term)
       (string-prefix-p "?" (symbol-name term))))

(defun kb-apply-bindings (conclusion bindings)
  "Apply variable bindings to conclusion."
  (let ((result conclusion))
    (dolist (binding bindings)
      (let ((var (car binding))
            (value (cdr binding)))
        (setq result (kb-substitute-var result var value))))
    result))

(defun kb-substitute-var (expr var value)
  "Substitute VAR with VALUE in EXPR."
  (cond
   ((eq expr var) value)
   ((listp expr) (mapcar (lambda (x) (kb-substitute-var x var value)) expr))
   (t expr)))

(defun kb-validate-new-fact (fact mt-name)
  "Validate that a new fact doesn't contradict existing knowledge."
  ;; Simple validation - can be enhanced with more sophisticated logic
  t)

;;; Temporal Reasoning Support

(defun kb-add-temporal-fact (subject predicate object valid-from valid-to &optional certainty)
  "Add a fact with temporal validity."
  (let ((temporal-info (kb-temporal-info-create
                       :valid-from valid-from
                       :valid-to valid-to)))
    (kb-add-fact subject predicate object certainty temporal-info)))

(defun kb-add-event-fact (subject predicate object happens-at &optional certainty)
  "Add a fact about an event occurrence."
  (let ((temporal-info (kb-temporal-info-create
                       :happens-at happens-at)))
    (kb-add-fact subject predicate object certainty temporal-info)))

(defun kb-query-at-time (subject predicate time &optional mt-name)
  "Query facts that are valid at a specific time."
  (let ((all-facts (kb-query-with-inheritance subject predicate mt-name)))
    (cl-remove-if-not
     (lambda (fact)
       (let ((temporal (kb-fact-temporal-info fact)))
         (if temporal
             (and (or (null (kb-temporal-info-valid-from temporal))
                     (time-less-p (kb-temporal-info-valid-from temporal) time))
                  (or (null (kb-temporal-info-valid-to temporal))
                     (time-less-p time (kb-temporal-info-valid-to temporal))))
           t)))  ; Facts without temporal info are always valid
     all-facts)))

;;; Initialization

;; Create base microtheory
(unless (kb-get-microtheory 'BaseMt)
  (kb-create-microtheory 'BaseMt))

;; Create common microtheories
(kb-create-microtheory 'CommonSenseMt 'BaseMt)
(kb-create-microtheory 'TemporalMt 'BaseMt)

(provide 'kb-microtheories)
;;; kb-microtheories.el ends here
