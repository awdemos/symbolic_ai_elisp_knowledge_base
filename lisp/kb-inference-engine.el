;;; kb-inference-engine.el --- Layered Inference Engine for Knowledge Base

;; Author: AI Assistant
;; Keywords: ai, inference, reasoning, microtheories
;; Version: 2.0

;;; Commentary:

;; This package implements a layered inference engine with
;; architecture with specialized inference workers and a strategist coordinator.

;;; Code:

(require 'cl-lib)
(require 'kb-microtheories)

;;; Inference Worker Structures

(cl-defstruct (kb-inference-worker (:constructor kb-inference-worker-create)
                                   (:copier nil))
  "A specialized inference worker."
  name
  type                    ; :fast, :medium, :general
  speciality             ; domain of expertise
  priority               ; execution priority
  cost                   ; computational cost estimate
  inference-fn           ; function to perform inference
  applicability-fn       ; function to check if worker applies
  active-p)              ; whether worker is currently active

(cl-defstruct (kb-inference-task (:constructor kb-inference-task-create)
                                 (:copier nil))
  "A task for the inference system."
  id
  query                  ; the query to answer
  microtheory           ; context microtheory
  priority              ; task priority
  timeout               ; max execution time
  partial-results       ; accumulated results
  assigned-workers      ; workers assigned to this task
  status)               ; :pending, :running, :completed, :failed

;;; Variables

(defvar kb-inference-workers nil
  "List of registered inference workers.")

(defvar kb-active-tasks (make-hash-table :test 'equal)
  "Hash table of active inference tasks.")

(defvar kb-inference-blackboard (make-hash-table :test 'equal)
  "Shared blackboard for worker coordination.")

(defvar kb-task-counter 0
  "Counter for generating unique task IDs.")

(defvar kb-inference-timeout 30.0
  "Default inference timeout in seconds.")

;;; Fast Inference Workers

(defun kb-create-direct-lookup-worker ()
  "Create worker for direct fact lookups."
  (kb-inference-worker-create
   :name 'direct-lookup
   :type :fast
   :speciality 'simple-query
   :priority 10
   :cost 1
   :inference-fn #'kb-direct-lookup-inference
   :applicability-fn #'kb-direct-lookup-applicable-p
   :active-p t))

(defun kb-direct-lookup-applicable-p (task)
  "Check if direct lookup applies to TASK."
  (let ((query (kb-inference-task-query task)))
    (and (listp query)
         (= (length query) 2)
         (not (kb-variable-p (car query)))
         (not (kb-variable-p (cadr query))))))

(defun kb-direct-lookup-inference (task)
  "Perform direct lookup inference for TASK."
  (let* ((query (kb-inference-task-query task))
         (subject (car query))
         (predicate (cadr query))
         (mt (kb-inference-task-microtheory task)))
    (kb-query-with-inheritance subject predicate mt)))

(defun kb-create-taxonomy-worker ()
  "Create worker for taxonomic reasoning."
  (kb-inference-worker-create
   :name 'taxonomy-reasoning
   :type :fast
   :speciality 'is-a-queries
   :priority 8
   :cost 3
   :inference-fn #'kb-taxonomy-inference
   :applicability-fn #'kb-taxonomy-applicable-p
   :active-p t))

(defun kb-taxonomy-applicable-p (task)
  "Check if taxonomic reasoning applies."
  (let ((query (kb-inference-task-query task)))
    (and (listp query)
         (eq (cadr query) 'is-a))))

(defun kb-taxonomy-inference (task)
  "Perform taxonomic inference."
  (let* ((query (kb-inference-task-query task))
         (subject (car query))
         (mt (kb-inference-task-microtheory task)))
    
    ;; Direct is-a facts
    (let ((direct-facts (kb-query-with-inheritance subject 'is-a mt))
          (inferred-facts nil))
      
      ;; Infer through class hierarchy
      (dolist (fact direct-facts)
        (let ((class (kb-fact-object fact)))
          (when (symbolp class)
            ;; Look for superclasses
            (let ((super-facts (kb-query-with-inheritance class 'is-a mt)))
              (dolist (super-fact super-facts)
                (let ((new-fact (kb-fact-create
                                :subject subject
                                :predicate 'is-a
                                :object (kb-fact-object super-fact)
                                :certainty (* (kb-fact-certainty fact)
                                            (kb-fact-certainty super-fact) 0.9)
                                :microtheory mt
                                :justification (list 'taxonomic-inference fact super-fact))))
                  (push new-fact inferred-facts)))))))
      
      (append direct-facts inferred-facts))))

;;; Medium Complexity Workers

(defun kb-create-rule-based-worker ()
  "Create worker for rule-based inference."
  (kb-inference-worker-create
   :name 'rule-based-inference
   :type :medium
   :speciality 'general-rules
   :priority 5
   :cost 8
   :inference-fn #'kb-rule-based-inference
   :applicability-fn #'kb-rule-based-applicable-p
   :active-p t))

(defun kb-rule-based-applicable-p (task)
  "Check if rule-based inference applies."
  (let ((mt (kb-get-microtheory (kb-inference-task-microtheory task))))
    (and mt (kb-microtheory-rules mt))))

(defun kb-rule-based-inference (task)
  "Perform rule-based inference."
  (let* ((mt-name (kb-inference-task-microtheory task))
         (mt (kb-get-microtheory mt-name))
         (query (kb-inference-task-query task))
         (results nil))
    
    (when mt
      (dolist (rule (kb-microtheory-rules mt))
        (let ((bindings (kb-match-rule-conclusion rule query)))
          (dolist (binding bindings)
            (when (kb-validate-rule-premises rule binding mt-name)
              (let ((new-fact (kb-instantiate-conclusion rule binding)))
                (push new-fact results)))))))
    
    results))

(defun kb-match-rule-conclusion (rule query)
  "Match rule conclusion against query pattern."
  (kb-unify (kb-rule-conclusion rule) query))

(defun kb-validate-rule-premises (rule binding mt-name)
  "Validate that rule premises are satisfied."
  (cl-every 
   (lambda (premise)
     (let ((instantiated (kb-apply-bindings premise (list binding))))
       (kb-query-with-inheritance 
        (car instantiated) (cadr instantiated) mt-name)))
   (kb-rule-premises rule)))

(defun kb-instantiate-conclusion (rule binding)
  "Create fact from rule conclusion and binding."
  (let ((conclusion (kb-apply-bindings (kb-rule-conclusion rule) (list binding))))
    (kb-fact-create
     :subject (car conclusion)
     :predicate (cadr conclusion)
     :object (caddr conclusion)
     :certainty 0.8
     :justification (list 'rule-inference rule binding))))

;;; Temporal Reasoning Worker

(defun kb-create-temporal-worker ()
  "Create worker for temporal reasoning."
  (kb-inference-worker-create
   :name 'temporal-reasoning
   :type :medium
   :speciality 'temporal-queries
   :priority 6
   :cost 5
   :inference-fn #'kb-temporal-inference
   :applicability-fn #'kb-temporal-applicable-p
   :active-p t))

(defun kb-temporal-applicable-p (task)
  "Check if temporal reasoning applies."
  (let ((query (kb-inference-task-query task)))
    (or (plist-get (cdr query) :at-time)
        (plist-get (cdr query) :during)
        (kb-has-temporal-facts-p (car query) (cadr query) 
                                (kb-inference-task-microtheory task)))))

(defun kb-has-temporal-facts-p (subject predicate mt-name)
  "Check if any facts have temporal information."
  (let ((facts (kb-query-with-inheritance subject predicate mt-name)))
    (cl-some (lambda (fact) (kb-fact-temporal-info fact)) facts)))

(defun kb-temporal-inference (task)
  "Perform temporal inference."
  (let* ((query (kb-inference-task-query task))
         (subject (car query))
         (predicate (cadr query))
         (time-constraint (plist-get (cdr query) :at-time))
         (mt-name (kb-inference-task-microtheory task)))
    
    (if time-constraint
        (kb-query-at-time subject predicate time-constraint mt-name)
      ;; Infer temporal relationships
      (kb-infer-temporal-relationships subject predicate mt-name))))

(defun kb-infer-temporal-relationships (subject predicate mt-name)
  "Infer temporal relationships between facts."
  (let ((facts (kb-query-with-inheritance subject predicate mt-name))
        (inferred nil))
    
    ;; Sort facts by temporal order
    (let ((temporal-facts (cl-remove-if-not #'kb-fact-temporal-info facts)))
      (dolist (fact temporal-facts)
        ;; Add temporal ordering inferences
        (let ((temporal (kb-fact-temporal-info fact)))
          (when (kb-temporal-info-happens-at temporal)
            ;; Infer before/after relationships
            (dolist (other-fact temporal-facts)
              (unless (eq fact other-fact)
                (let ((other-temporal (kb-fact-temporal-info other-fact)))
                  (when (and other-temporal 
                           (kb-temporal-info-happens-at other-temporal))
                    (when (time-less-p (kb-temporal-info-happens-at temporal)
                                     (kb-temporal-info-happens-at other-temporal))
                      (let ((before-fact 
                             (kb-fact-create
                              :subject (kb-fact-object fact)
                              :predicate 'happens-before
                              :object (kb-fact-object other-fact)
                              :certainty 1.0
                              :microtheory mt-name
                              :justification (list 'temporal-ordering fact other-fact))))
                        (push before-fact inferred)))))))))))
    
    (append facts inferred)))

;;; General Inference Worker

(defun kb-create-general-worker ()
  "Create general-purpose inference worker (expensive)."
  (kb-inference-worker-create
   :name 'general-inference
   :type :general
   :speciality 'any
   :priority 1
   :cost 50
   :inference-fn #'kb-general-inference
   :applicability-fn (lambda (task) t)  ; Always applicable
   :active-p nil))  ; Disabled by default due to cost

(defun kb-general-inference (task)
  "Perform expensive general inference."
  ;; This would implement more sophisticated reasoning
  ;; For now, just return empty results to avoid infinite computation
  nil)

;;; Inference Strategist

(defun kb-inference-strategist (query microtheory &optional timeout)
  "Coordinate inference workers to answer a query."
  (let* ((task-id (cl-incf kb-task-counter))
         (task (kb-inference-task-create
                :id task-id
                :query query
                :microtheory microtheory
                :priority 1.0
                :timeout (or timeout kb-inference-timeout)
                :partial-results nil
                :assigned-workers nil
                :status :pending))
         (start-time (current-time))
         (results nil))
    
    (puthash task-id task kb-active-tasks)
    
    ;; Try workers in priority order
    (let ((applicable-workers 
           (cl-sort
            (cl-remove-if-not 
             (lambda (worker) 
               (and (kb-inference-worker-active-p worker)
                    (funcall (kb-inference-worker-applicability-fn worker) task)))
             kb-inference-workers)
            (lambda (w1 w2) 
              (> (kb-inference-worker-priority w1)
                 (kb-inference-worker-priority w2))))))
      
      (dolist (worker applicable-workers)
        (when (and (null results)
                  (< (float-time (time-subtract (current-time) start-time))
                     (kb-inference-task-timeout task)))
          
          ;; Try this worker
          (setf (kb-inference-task-status task) :running)
          (push worker (kb-inference-task-assigned-workers task))
          
          (condition-case err
              (let ((worker-results 
                     (funcall (kb-inference-worker-inference-fn worker) task)))
                (when worker-results
                  (setq results worker-results)
                  (setf (kb-inference-task-status task) :completed)))
            (error 
             (message "Inference worker %s failed: %s" 
                     (kb-inference-worker-name worker) err))))))
    
    ;; Clean up
    (remhash task-id kb-active-tasks)
    results))

;;; Utility Functions

(defun kb-unify (pattern query)
  "Simple unification between pattern and query."
  (cond
   ((equal pattern query) (list nil))  ; Empty binding
   ((and (listp pattern) (listp query) (= (length pattern) (length query)))
    (let ((bindings (list nil)))
      (cl-loop for p-elem in pattern
               for q-elem in query
               do (setq bindings 
                       (cl-mapcan 
                        (lambda (binding)
                          (let ((unified (kb-unify-terms p-elem q-elem binding)))
                            (if unified (list unified) nil)))
                        bindings)))
      bindings))
   (t nil)))

(defun kb-unify-terms (term1 term2 binding)
  "Unify two terms with existing binding."
  (cond
   ((equal term1 term2) binding)
   ((kb-variable-p term1)
    (let ((val (cdr (assoc term1 binding))))
      (if val
          (if (equal val term2) binding nil)
        (cons (cons term1 term2) binding))))
   ((kb-variable-p term2)
    (let ((val (cdr (assoc term2 binding))))
      (if val
          (if (equal val term1) binding nil)
        (cons (cons term2 term1) binding))))
   (t nil)))

;;; Initialization

(defun kb-init-inference-engine ()
  "Initialize the inference engine with standard workers."
  (setq kb-inference-workers
        (list (kb-create-direct-lookup-worker)
              (kb-create-taxonomy-worker)
              (kb-create-temporal-worker)
              (kb-create-rule-based-worker)
              (kb-create-general-worker)))
  (clrhash kb-active-tasks)
  (clrhash kb-inference-blackboard))

;; Initialize on load
(kb-init-inference-engine)

(provide 'kb-inference-engine)
;;; kb-inference-engine.el ends here
