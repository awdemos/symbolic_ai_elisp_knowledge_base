;;; kb-microtheories.el --- Microtheory System with Inheritance for Knowledge Base

;; Author: AI Assistant  
;; Keywords: ai, knowledge base, microtheories, context, inheritance
;; Version: 3.0

;;; Commentary:

;; This package implements a microtheory system for managing
;; contextual knowledge with proper inheritance semantics,
;; including multiple inheritance, conflict resolution,
;; cycle detection, and fact shadowing.

;;; Code:

(require 'cl-lib)
(require 'kb-tms)
(require 'kb-validation)

;;; Microtheory Structures

(cl-defstruct (kb-microtheory (:constructor kb-microtheory-create)
                              (:copier nil))
  "A microtheory context for scoped knowledge."
  name
  parent-mts        ; list of direct parent microtheories  
  facts            ; hash table of facts in this mt
  rules            ; list of inference rules in this mt
  inherits-from    ; list of mts to inherit from (deprecated, use parent-mts)
  temp-p           ; t if this is a temporary microtheory
  created-at       ; timestamp when created
  priority         ; priority for conflict resolution (higher = more specific)
  inheritance-mode ; 'strict, 'override, 'merge
  local-facts)     ; facts that should not be inherited

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

(defvar kb-inheritance-cache (make-hash-table :test 'equal)
  "Cache for inheritance chains to improve performance.")

(defvar kb-cycle-detection-stack nil
  "Stack for detecting cycles during inheritance traversal.")

;;; Core Microtheory Functions

(defun kb-create-microtheory (name &optional parent-mts priority inheritance-mode)
  "Create a new microtheory with NAME and optional PARENT-MTS.
PRIORITY determines specificity for conflict resolution (higher = more specific).
INHERITANCE-MODE can be 'strict, 'override, or 'merge."
  (kb-with-validation kb-create-microtheory (list name parent-mts priority inheritance-mode)
    (kb-with-error-recovery
      (let* ((parent-list (cond
                          ((null parent-mts) nil)
                          ((listp parent-mts) parent-mts)
                          (t (list parent-mts))))
             (priority (or priority 
                          (if parent-list 
                              (condition-case nil
                                  (1+ (apply #'max (mapcar #'kb-get-microtheory-priority parent-list)))
                                (error 1)) ; fallback priority if parent priorities unavailable
                            0)))
             (mt (kb-microtheory-create
                  :name name
                  :parent-mts parent-list
                  :facts (make-hash-table :test 'equal)
                  :rules nil
                  :inherits-from parent-list ; backward compatibility
                  :temp-p nil
                  :created-at (current-time)
                  :priority priority
                  :inheritance-mode (or inheritance-mode 'merge)
                  :local-facts (make-hash-table :test 'equal))))
        
        ;; Validate inheritance hierarchy doesn't create cycles
        (condition-case err
            (kb-validate-inheritance-cycle name parent-list)
          (error 
           (signal 'kb-microtheory-error 
                   (list "Inheritance cycle detected" name parent-list))))
        
        ;; Clear inheritance cache since hierarchy changed
        (kb-clear-inheritance-cache)
        
        (puthash name mt kb-microtheories)
        mt))))

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

;;; Inheritance and Cycle Detection Functions

(defun kb-get-microtheory-priority (mt-name)
  "Get the priority of microtheory MT-NAME."
  (let ((mt (kb-get-microtheory mt-name)))
    (if mt (kb-microtheory-priority mt) 0)))

(defun kb-validate-inheritance-cycle (mt-name parent-mts)
  "Validate that adding PARENT-MTS to MT-NAME doesn't create cycles."
  (let ((kb-cycle-detection-stack (list mt-name)))
    (dolist (parent parent-mts)
      (when (kb-has-inheritance-cycle-p parent (list mt-name))
        (error "Adding parent %s to %s would create inheritance cycle" parent mt-name)))))

(defun kb-has-inheritance-cycle-p (mt-name visited)
  "Check if MT-NAME has inheritance cycles, given VISITED microtheories."
  (if (memq mt-name visited)
      t
    (let ((mt (kb-get-microtheory mt-name)))
      (when mt
        (let ((new-visited (cons mt-name visited)))
          (cl-some (lambda (parent) 
                     (kb-has-inheritance-cycle-p parent new-visited))
                   (kb-microtheory-parent-mts mt)))))))

(defun kb-clear-inheritance-cache ()
  "Clear the inheritance cache."
  (setq kb-inheritance-cache (make-hash-table :test 'equal)))

(defun kb-get-inheritance-chain (mt-name)
  "Get the complete inheritance chain for MT-NAME with caching."
  (or (gethash mt-name kb-inheritance-cache)
      (let ((chain (kb-compute-inheritance-chain mt-name)))
        (puthash mt-name chain kb-inheritance-cache)
        chain)))

(defun kb-compute-inheritance-chain (mt-name &optional visited)
  "Compute the inheritance chain for MT-NAME using topological sort."
  (when (memq mt-name visited)
    (error "Inheritance cycle detected involving %s" mt-name))
  
  (let* ((mt (kb-get-microtheory mt-name))
         (visited (cons mt-name visited))
         (parents (if mt (kb-microtheory-parent-mts mt) nil))
         (chain (list mt-name)))
    
    ;; Add all ancestor chains
    (dolist (parent parents)
      (let ((parent-chain (kb-compute-inheritance-chain parent visited)))
        (setq chain (append chain parent-chain))))
    
    ;; Remove duplicates while preserving order, prioritize more specific first
    (kb-resolve-inheritance-order (cl-remove-duplicates chain :test 'eq))))

(defun kb-resolve-inheritance-order (mt-names)
  "Sort MT-NAMES by inheritance priority (most specific first)."
  (sort mt-names
        (lambda (a b)
          (let ((priority-a (kb-get-microtheory-priority a))
                (priority-b (kb-get-microtheory-priority b)))
            (> priority-a priority-b)))))

(defun kb-add-parent-microtheory (child-name parent-name)
  "Add PARENT-NAME as a parent of CHILD-NAME."
  (let ((child-mt (kb-get-microtheory child-name)))
    (unless child-mt
      (error "Child microtheory %s does not exist" child-name))
    (unless (kb-get-microtheory parent-name)
      (error "Parent microtheory %s does not exist" parent-name))
    
    ;; Check for cycles
    (kb-validate-inheritance-cycle child-name (list parent-name))
    
    ;; Add parent if not already present
    (unless (memq parent-name (kb-microtheory-parent-mts child-mt))
      (push parent-name (kb-microtheory-parent-mts child-mt))
      (push parent-name (kb-microtheory-inherits-from child-mt))
      (kb-clear-inheritance-cache))))

(defun kb-remove-parent-microtheory (child-name parent-name)
  "Remove PARENT-NAME as a parent of CHILD-NAME."
  (let ((child-mt (kb-get-microtheory child-name)))
    (when child-mt
      (setf (kb-microtheory-parent-mts child-mt)
            (cl-remove parent-name (kb-microtheory-parent-mts child-mt)))
      (setf (kb-microtheory-inherits-from child-mt)
            (cl-remove parent-name (kb-microtheory-inherits-from child-mt)))
      (kb-clear-inheritance-cache))))

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

;;; Enhanced Fact Management with Microtheories and TMS

(defun kb-add-fact (subject predicate object &optional certainty temporal-info local-p)
  "Add a fact to the current microtheory with TMS tracking.
If LOCAL-P is t, the fact won't be inherited by child microtheories."
  (kb-with-error-recovery
    ;; Validate inputs
    (kb-validate-fact-structure subject predicate object)
    (kb-validate-certainty certainty)
    (kb-validate-temporal-info temporal-info)
    
    (let* ((mt (kb-get-microtheory kb-current-mt))
           (certainty (or certainty 1.0)))
      
      (unless mt
        (signal 'kb-microtheory-error (list "Current microtheory not found" kb-current-mt)))
      
      (let* ((fact (kb-fact-create 
                    :subject subject
                    :predicate predicate
                    :object object
                    :certainty certainty
                    :microtheory kb-current-mt
                    :temporal-info temporal-info))
             (facts (gethash subject (kb-microtheory-facts mt))))
        
        ;; Add to microtheory fact store
        (puthash subject (cons fact facts) (kb-microtheory-facts mt))
        
        ;; Mark as local if specified
        (when local-p
          (puthash (list subject predicate object) t (kb-microtheory-local-facts mt)))
        
        ;; Add to TMS with error handling
        (condition-case err
            (kb-tms-assert-fact subject predicate object kb-current-mt nil nil 'direct)
          (error 
           (message "TMS assertion failed: %s" (error-message-string err))))
        
        fact))))

(defun kb-add-fact-with-justification (subject predicate object premises rule &optional certainty)
  "Add a derived fact with explicit justification."
  (let* ((mt (kb-get-microtheory kb-current-mt))
         (certainty (or certainty 1.0))
         (fact (kb-fact-create 
                :subject subject
                :predicate predicate
                :object object
                :certainty certainty
                :microtheory kb-current-mt
                :justification (list :premises premises :rule rule)))
         (facts (gethash subject (kb-microtheory-facts mt))))
    
    ;; Add to microtheory
    (puthash subject (cons fact facts) (kb-microtheory-facts mt))
    
    ;; Add to TMS with justification
    (kb-tms-assert-fact subject predicate object kb-current-mt premises rule 'derived)
    
    fact))

(defun kb-retract-fact (subject predicate object &optional mt-name)
  "Retract a fact from the specified microtheory (or current)."
  (let* ((mt-name (or mt-name kb-current-mt))
         (mt (kb-get-microtheory mt-name)))
    (when mt
      ;; Remove from microtheory fact store
      (let* ((facts (gethash subject (kb-microtheory-facts mt)))
             (remaining (cl-remove-if 
                        (lambda (fact)
                          (and (eq (kb-fact-predicate fact) predicate)
                               (equal (kb-fact-object fact) object)))
                        facts)))
        (if remaining
            (puthash subject remaining (kb-microtheory-facts mt))
          (remhash subject (kb-microtheory-facts mt))))
      
      ;; Retract from TMS
      (kb-tms-retract-fact subject predicate object mt-name))))

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
  "Query facts with proper microtheory inheritance and shadowing."
  (let* ((mt-name (or mt-name kb-current-mt))
         (inheritance-chain (kb-get-inheritance-chain mt-name))
         (all-results nil)
         (shadowed-facts (make-hash-table :test 'equal)))
    
    ;; Collect facts from inheritance chain (most specific first)
    (dolist (current-mt inheritance-chain)
      (let* ((current-results (kb-query-in-mt subject predicate current-mt))
             (mt (kb-get-microtheory current-mt)))
        
        ;; Filter out facts that should not be inherited
        (when (and mt current-results)
          (setq current-results
                (cl-remove-if 
                 (lambda (fact)
                   (gethash (list (kb-fact-subject fact) 
                                 (kb-fact-predicate fact) 
                                 (kb-fact-object fact))
                           (kb-microtheory-local-facts mt)))
                 current-results)))
        
        ;; Add non-shadowed facts
        (dolist (fact current-results)
          (let ((fact-key (list (kb-fact-object fact))))
            (unless (gethash fact-key shadowed-facts)
              (push fact all-results)
              ;; Mark this object as shadowed for less specific MTs
              (puthash fact-key t shadowed-facts))))))
    
    ;; Apply conflict resolution based on inheritance mode
    (kb-resolve-inheritance-conflicts all-results mt-name)))

(defun kb-resolve-inheritance-conflicts (facts mt-name)
  "Resolve conflicts between inherited facts based on inheritance mode."
  (let* ((mt (kb-get-microtheory mt-name))
         (mode (if mt (kb-microtheory-inheritance-mode mt) 'merge))
         (grouped-facts (make-hash-table :test 'equal)))
    
    ;; Group facts by object value
    (dolist (fact facts)
      (let ((key (kb-fact-object fact)))
        (push fact (gethash key grouped-facts))))
    
    ;; Apply resolution strategy
    (let ((resolved-facts nil))
      (maphash 
       (lambda (key fact-group)
         (setq resolved-facts 
               (append resolved-facts 
                       (kb-apply-conflict-resolution fact-group mode))))
       grouped-facts)
      resolved-facts)))

(defun kb-apply-conflict-resolution (fact-group mode)
  "Apply conflict resolution strategy to a group of conflicting facts."
  (cond
   ((eq mode 'strict)
    ;; Only keep facts from most specific microtheory
    (let ((max-priority (apply #'max 
                              (mapcar (lambda (fact)
                                       (kb-get-microtheory-priority 
                                        (kb-fact-microtheory fact)))
                                     fact-group))))
      (cl-remove-if-not 
       (lambda (fact)
         (= (kb-get-microtheory-priority (kb-fact-microtheory fact)) max-priority))
       fact-group)))
   
   ((eq mode 'override)
    ;; Child facts completely override parent facts
    (list (car (sort fact-group 
                    (lambda (a b)
                      (> (kb-get-microtheory-priority (kb-fact-microtheory a))
                         (kb-get-microtheory-priority (kb-fact-microtheory b))))))))
   
   (t ; 'merge mode (default)
    ;; Keep all facts, weighted by certainty and microtheory priority
    (sort fact-group 
          (lambda (a b)
            (let ((score-a (* (kb-fact-certainty a) 
                             (1+ (kb-get-microtheory-priority (kb-fact-microtheory a)))))
                  (score-b (* (kb-fact-certainty b) 
                             (1+ (kb-get-microtheory-priority (kb-fact-microtheory b))))))
              (> score-a score-b)))))))

(defun kb-query-with-tms-check (subject predicate &optional mt-name)
  "Query facts with inheritance, filtering by TMS belief status."
  (let ((candidates (kb-query-with-inheritance subject predicate mt-name)))
    (cl-remove-if-not 
     (lambda (fact)
       (kb-tms-is-believed (kb-fact-subject fact)
                          (kb-fact-predicate fact)
                          (kb-fact-object fact)
                          (kb-fact-microtheory fact)))
     candidates)))

(defun kb-fact-equal-p (fact1 fact2)
  "Check if two facts are equal."
  (and (eq (kb-fact-subject fact1) (kb-fact-subject fact2))
       (eq (kb-fact-predicate fact1) (kb-fact-predicate fact2))
       (equal (kb-fact-object fact1) (kb-fact-object fact2))))

;;; Inheritance Chain Query Functions

(defun kb-get-ancestors (mt-name)
  "Get all ancestor microtheories of MT-NAME."
  (let ((chain (kb-get-inheritance-chain mt-name)))
    (cdr chain))) ; Remove self

(defun kb-get-descendants (mt-name)
  "Get all descendant microtheories of MT-NAME."
  (let ((descendants nil))
    (maphash (lambda (name mt)
               (when (and (not (eq name mt-name))
                         (memq mt-name (kb-get-inheritance-chain name)))
                 (push name descendants)))
             kb-microtheories)
    descendants))

(defun kb-is-ancestor-p (ancestor-name descendant-name)
  "Check if ANCESTOR-NAME is an ancestor of DESCENDANT-NAME."
  (memq ancestor-name (kb-get-ancestors descendant-name)))

(defun kb-get-inheritance-path (from-mt to-mt)
  "Get the inheritance path from FROM-MT to TO-MT, if it exists."
  (let ((from-chain (kb-get-inheritance-chain from-mt)))
    (when (memq to-mt from-chain)
      (let ((path nil)
            (found nil))
        (dolist (mt from-chain)
          (push mt path)
          (when (eq mt to-mt)
            (setq found t)
            (return)))
        (when found (nreverse path))))))

(defun kb-find-common-ancestors (mt1 mt2)
  "Find common ancestor microtheories of MT1 and MT2."
  (let ((ancestors1 (kb-get-ancestors mt1))
        (ancestors2 (kb-get-ancestors mt2)))
    (cl-intersection ancestors1 ancestors2 :test 'eq)))

(defun kb-get-microtheory-info (mt-name)
  "Get comprehensive information about a microtheory."
  (let ((mt (kb-get-microtheory mt-name)))
    (when mt
      (list :name mt-name
            :priority (kb-microtheory-priority mt)
            :inheritance-mode (kb-microtheory-inheritance-mode mt)
            :parents (kb-microtheory-parent-mts mt)
            :ancestors (kb-get-ancestors mt-name)
            :descendants (kb-get-descendants mt-name)
            :fact-count (hash-table-count (kb-microtheory-facts mt))
            :rule-count (length (kb-microtheory-rules mt))
            :local-fact-count (hash-table-count (kb-microtheory-local-facts mt))
            :temp-p (kb-microtheory-temp-p mt)
            :created-at (kb-microtheory-created-at mt)))))

(defun kb-explain-inheritance (subject predicate mt-name)
  "Explain how a fact is inherited in the microtheory hierarchy."
  (let ((inheritance-chain (kb-get-inheritance-chain mt-name))
        (explanations nil))
    
    (dolist (current-mt inheritance-chain)
      (let ((local-facts (kb-query-in-mt subject predicate current-mt)))
        (when local-facts
          (push (list :microtheory current-mt
                     :priority (kb-get-microtheory-priority current-mt)
                     :facts local-facts
                     :source (if (eq current-mt mt-name) 'local 'inherited))
                explanations))))
    
    (nreverse explanations)))

(defun kb-list-microtheories (&optional include-temp)
  "List all microtheories, optionally including temporary ones."
  (let ((result nil))
    (maphash (lambda (name mt)
               (when (or include-temp (not (kb-microtheory-temp-p mt)))
                 (push name result)))
             kb-microtheories)
    (sort result (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(defun kb-microtheory-hierarchy ()
  "Return a hierarchical representation of all microtheories."
  (let ((hierarchy (make-hash-table :test 'equal))
        (roots nil))
    
    ;; Build hierarchy
    (maphash (lambda (name mt)
               (let ((parents (kb-microtheory-parent-mts mt)))
                 (if parents
                     (dolist (parent parents)
                       (push name (gethash parent hierarchy)))
                   (push name roots))))
             kb-microtheories)
    
    (list :roots roots :hierarchy hierarchy)))

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

;;; Initialization and Validation

(defun kb-microtheory-validate ()
  "Validate the consistency of the microtheory system."
  (let ((errors nil))
    
    ;; Check for cycles in inheritance hierarchy
    (maphash (lambda (name mt)
               (condition-case err
                   (kb-get-inheritance-chain name)
                 (error (push (format "Inheritance cycle involving %s: %s" 
                                    name (error-message-string err)) errors))))
             kb-microtheories)
    
    ;; Check that all parent microtheories exist
    (maphash (lambda (name mt)
               (dolist (parent (kb-microtheory-parent-mts mt))
                 (unless (kb-get-microtheory parent)
                   (push (format "Missing parent microtheory %s for %s" parent name) errors))))
             kb-microtheories)
    
    ;; Validate TMS consistency
    (let ((tms-result (kb-tms-validate)))
      (when (and (listp tms-result) (eq (car tms-result) 'errors))
        (setq errors (append errors (cdr tms-result)))))
    
    (if errors
        (list 'errors errors)
      'valid)))

(defun kb-microtheory-status ()
  "Return status information about the microtheory system."
  (let ((mt-count (hash-table-count kb-microtheories))
        (temp-count 0)
        (total-facts 0)
        (total-rules 0))
    
    (maphash (lambda (name mt)
               (when (kb-microtheory-temp-p mt)
                 (cl-incf temp-count))
               (cl-incf total-facts (hash-table-count (kb-microtheory-facts mt)))
               (cl-incf total-rules (length (kb-microtheory-rules mt))))
             kb-microtheories)
    
    (format "Microtheory System: %d microtheories (%d temp), %d facts, %d rules"
            mt-count temp-count total-facts total-rules)))

;;; Initialization

;; Initialize TMS
(kb-tms-init)

;; Create base microtheory
(unless (kb-get-microtheory 'BaseMt)
  (kb-create-microtheory 'BaseMt nil 0 'merge))

;; Create common microtheories with proper inheritance
(unless (kb-get-microtheory 'CommonSenseMt)
  (kb-create-microtheory 'CommonSenseMt 'BaseMt 1 'merge))

(unless (kb-get-microtheory 'TemporalMt)
  (kb-create-microtheory 'TemporalMt 'BaseMt 1 'override))

(provide 'kb-microtheories)
;;; kb-microtheories.el ends here
