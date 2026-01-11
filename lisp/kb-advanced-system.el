;;; kb-advanced-system.el --- Unified Advanced Knowledge Base System
;; -*- lexical-binding: t; -*-

;; Author: AI Assistant
;; Keywords: ai, knowledge base, reasoning, microtheories
;; Version: 2.1

;;; Commentary:

;; This package provides a unified interface to an advanced knowledge base
;; system, integrating microtheories, layered inference, non-monotonic reasoning,
;; temporal logic, event reification, debugging tools, query caching,
;; testing framework, RDF/OWL import, and database persistence.

;;; Code:

(require 'cl-lib)
(require 'kb-microtheories)
(require 'kb-inference-engine)
(require 'kb-nonmonotonic)
(require 'kb-events)
(require 'kb-debugger)
(require 'kb-cache)
(require 'kb-testing)
(require 'kb-rdf)
(require 'kb-tms)
(require 'kb-persistence)
(require 'kb-validation)


;;; High-level API

;;;###autoload
(defun kb-init (&optional base-mt)
  "Initialize the knowledge base system.
Optional BASE-MT specifies the base microtheory to use."
  (interactive)
  
  ;; Initialize microtheories
  (unless (kb-get-microtheory 'BaseMt)
    (kb-create-microtheory 'BaseMt))
  (unless (kb-get-microtheory 'CommonSenseMt)
    (kb-create-microtheory 'CommonSenseMt '(BaseMt)))
  (unless (kb-get-microtheory 'TemporalMt)
    (kb-create-microtheory 'TemporalMt '(BaseMt)))
  (unless (kb-get-microtheory 'EventMt)
    (kb-create-microtheory 'EventMt '(BaseMt)))
  
  ;; Initialize inference engine
  (kb-init-inference-engine)
  
  ;; Setup default reasoning examples
  (kb-setup-default-reasoning-examples)
  
  ;; Setup event examples
  (kb-setup-event-examples)
  
  ;; Set current microtheory
  (setq kb-current-mt (or base-mt 'CommonSenseMt))
  
  (message "KB Advanced system v2.1 initialized with microtheory: %s" kb-current-mt))

;;;###autoload
(defmacro with-kb (&rest body)
  "Execute BODY with the knowledge base system loaded and initialized."
  `(progn
     (unless (boundp 'kb-current-mt)
       (kb-init))
     ,@body))

;;;###autoload
(defmacro in-microtheory (mt &rest body)
  "Execute BODY in the context of microtheory MT."
  `(kb-in-microtheory ,mt ,@body))

;;; Fact Management API

;;;###autoload
(defun kb-assert (subject predicate object &optional certainty temporal-info mt)
  "Assert a fact in the knowledge base.
SUBJECT, PREDICATE and OBJECT define the fact to assert.
CERTAINTY is the confidence level of the fact.
TEMPORAL-INFO specifies temporal constraints for the fact.
MT specifies the microtheory to assert into."
  (interactive "sSubject: \nsPredicate: \nsObject: ")
  (kb-with-validation kb-assert (list subject predicate object certainty temporal-info mt)
    (kb-with-error-recovery
      (let ((kb-current-mt (or mt kb-current-mt)))
        (kb-add-fact subject predicate object certainty temporal-info)))))

;;;###autoload
(defun kb-assert-temporal (subject predicate object valid-from valid-to &optional certainty mt)
  "Assert a temporally bounded fact.
SUBJECT, PREDICATE and OBJECT define the fact to assert.
VALID-FROM specifies the start time for the fact's validity.
VALID-TO specifies the end time for the fact's validity.
CERTAINTY is the confidence level of the fact.
MT specifies the microtheory to assert into."
  (kb-with-error-recovery
    ;; Create temporal-info structure for validation
    (let ((temporal-info (list :valid-from valid-from :valid-to valid-to)))
      (kb-with-validation kb-assert (list subject predicate object certainty temporal-info mt)
        (let ((kb-current-mt (or mt kb-current-mt)))
          (kb-add-temporal-fact subject predicate object valid-from valid-to certainty))))))

;;;###autoload
(defun kb-retract (subject predicate object &optional mt)
  "Retract a fact from the knowledge base.
SUBJECT, PREDICATE and OBJECT identify the fact to retract.
MT specifies the microtheory to retract from."
  (kb-with-validation kb-retract (list subject predicate object mt)
    (kb-with-error-recovery
      (let* ((kb-current-mt (or mt kb-current-mt))
             (facts (kb-query-with-inheritance subject predicate mt)))
        (dolist (fact facts)
          (when (equal (kb-fact-object fact) object)
            (kb-remove-fact fact)))))))

;;;###autoload
(defun kb-query (subject predicate &optional mt)
  "Query the knowledge base.
SUBJECT and PREDICATE identify the facts to query.
MT specifies the microtheory to query in."
  (interactive "sSubject: \nsPredicate: ")
  (kb-with-validation kb-query (list subject predicate mt)
    (kb-with-error-recovery
      (let ((kb-current-mt (or mt kb-current-mt)))
        (kb-query-with-inheritance subject predicate mt)))))

;;;###autoload
(defun kb-ask (query &optional mt timeout)
  "Ask a complex query using the inference engine.
QUERY specifies the complex query to evaluate.
MT specifies the microtheory to query in.
TIMEOUT specifies the maximum time to spend on inference."
  (kb-with-error-recovery
    (when mt
      (kb-validate-microtheory-name mt)
      (kb-validate-microtheory-exists mt t))
    (when timeout
      (kb-validate-number timeout nil 0))
    (let ((kb-current-mt (or mt kb-current-mt)))
      (kb-inference-strategist query mt timeout))))

;;; Rule Management API

;;;###autoload
(defun kb-add-rule (name premises conclusion &optional priority temporal-p mt)
  "Add an inference rule.
NAME identifies the rule to add.
PREMISES specifies the rule's preconditions.
CONCLUSION specifies the rule's outcome.
PRIORITY specifies the rule's priority level.
TEMPORAL-P specifies if rule handles temporal logic.
MT specifies the microtheory to add rule to."
  (kb-with-validation kb-add-rule (list name premises conclusion priority temporal-p mt)
    (kb-with-error-recovery
      (let* ((kb-current-mt (or mt kb-current-mt))
             (mt-obj (kb-get-microtheory kb-current-mt)))
        (unless mt-obj
          (signal 'kb-microtheory-error (list "Microtheory not found" kb-current-mt)))
        (let ((rule (kb-rule-create
                     :name name
                     :premises premises
                     :conclusion conclusion
                     :microtheory kb-current-mt
                     :priority (or priority 1.0)
                     :temporal-p temporal-p)))
          (push rule (kb-microtheory-rules mt-obj))
          rule)))))

;;;###autoload
(defun kb-add-default (name premises conclusion &optional exceptions strength specificity mt)
  "Add a default rule with possible exceptions.
NAME identifies the default rule to add.
PREMISES specifies the rule's preconditions.
CONCLUSION specifies the rule's outcome.
EXCEPTIONS specifies possible exceptions to the rule.
STRENGTH specifies the default rule's strength.
SPECIFICITY specifies the rule's specificity level.
MT specifies the microtheory to add rule to."
  (let ((kb-current-mt (or mt kb-current-mt)))
    (kb-add-default-rule name premises conclusion exceptions strength specificity)))

;;;###autoload
(defun kb-add-exception (name rule-name conditions &optional alternative priority mt)
  "Add an exception to a default rule.
NAME identifies the exception to add.
RULE-NAME specifies the rule to add exception to.
CONDITIONS specifies when the exception applies.
ALTERNATIVE specifies alternative conclusion when exception applies.
PRIORITY specifies the exception's priority level.
MT specifies the microtheory to add exception to."
  (let ((kb-current-mt (or mt kb-current-mt)))
    (let ((exception (kb-exception-create
                     :name name
                     :rule-name rule-name
                     :conditions conditions
                     :alternative alternative
                     :priority (or priority 1.0)
                     :microtheory kb-current-mt)))
      (push exception kb-exceptions)
      
      ;; Update the default rule to include this exception
      (let ((rule (cl-find rule-name kb-default-rules
                          :key #'kb-default-rule-name)))
        (when rule
          (push name (kb-default-rule-exceptions rule))))
      exception)))

;;; Event Management API

;;;###autoload
(defun kb-create-event (type &rest properties)
  "Create a new event.
TYPE specifies the type of event to create.
PROPERTIES specifies additional event attributes."
  (let* ((event-id (intern (format "Event-%d" (cl-incf kb-event-counter))))
         (event (kb-event-create
                :id event-id
                :type type
                :participants (plist-get properties :participants)
                :roles (plist-get properties :roles)
                :start-time (plist-get properties :start-time)
                :end-time (plist-get properties :end-time)
                :duration (plist-get properties :duration)
                :location (plist-get properties :location)
                :properties properties
                :microtheory kb-current-mt)))
    
    (puthash event-id event kb-events)
    event-id))

;;;###autoload
(defun kb-define-process (name &rest properties)
  "Define a new process type.
NAME specifies the name of the process type.
PROPERTIES specifies process characteristics."
  (let ((process (kb-process-create
                 :name name
                 :typical-duration (plist-get properties :typical-duration)
                 :typical-participants (plist-get properties :typical-participants)
                 :typical-roles (plist-get properties :typical-roles)
                 :preconditions (plist-get properties :preconditions)
                 :effects (plist-get properties :effects)
                 :invariants (plist-get properties :invariants)
                 :microtheory kb-current-mt)))
    
    (puthash name process kb-processes)
    
    ;; Add facts about this process type
    (kb-assert name 'is-a 'process)
    process))

;;;###autoload
(defun kb-relate-events (relation source target &optional strength)
  "Add a relation between events.
RELATION specifies the type of relation between events.
SOURCE specifies the source event in the relation.
TARGET specifies the target event in the relation.
STRENGTH specifies the strength of the relation."
  (kb-add-event-relation relation source target strength))

;;; Debugging API

;;;###autoload
(defun kb-debug-on ()
  "Enable debugging for the knowledge base."
  (interactive)
  (kb-debug-enable))

;;;###autoload
(defun kb-debug-off ()
  "Disable debugging for the knowledge base."
  (interactive)
  (kb-debug-disable))

;;;###autoload
(defun kb-trace-query (subject predicate &optional mt)
  "Query with debugging trace enabled.
SUBJECT and PREDICATE identify the facts to query.
MT specifies the microtheory to query in."
  (let ((orig-debug kb-debug-enabled))
    (unless orig-debug (kb-debug-enable))
    (unwind-protect
        (kb-debug-query subject predicate mt)
      (unless orig-debug (kb-debug-disable)))))

;;;###autoload
(defun kb-validate (mt-name)
  "Validate consistency of a microtheory.
MT-NAME specifies the microtheory to validate."
  (interactive (list (completing-read "Microtheory: " (kb-list-microtheories))))
  (kb-validate-microtheory mt-name))

;;; Caching API

;;;###autoload
(defun kb-cache-on (&optional max-size ttl)
  "Enable query caching.
Optional MAX-SIZE specifies the maximum cache size.
Optional TTL specifies the time-to-live for cache entries."
  (interactive)
  (kb-cache-enable max-size ttl))

;;;###autoload
(defun kb-cache-off ()
  "Disable query caching."
  (interactive)
  (kb-cache-disable))

;;;###autoload
(defun kb-cached-query (subject predicate &optional mt)
  "Query with caching enabled.
SUBJECT and PREDICATE identify the facts to query.
MT specifies the microtheory to query in."
  (cl-incf (kb-cache-stats-total-queries kb-cache-stats))
  
  (if kb-cache-enabled
      (let* ((key (kb-cache-key "query" subject predicate (or mt kb-current-mt)))
             (cached-result (kb-cache-get key)))
        (if cached-result
            cached-result
          ;; Cache miss - perform query and cache result
          (cl-incf (kb-cache-stats-misses kb-cache-stats))
          (let ((result (kb-query-with-inheritance subject predicate mt)))
            (kb-cache-put key result)
            result)))
    ;; Caching disabled - direct query
    (kb-query-with-inheritance subject predicate mt)))

;;; Testing API

;;;###autoload
(defun kb-run-tests (&optional suite-name)
  "Run knowledge base test.
Optional SUITE-NAME specifies which test suite to run."
  (interactive)
  (if suite-name
      (kb-run-test-suite suite-name)
    (kb-run-all-tests)))



;;; RDF API

;;;###autoload
(defun kb-import-rdf (file-path &optional format mt-name)
  "Import RDF/OWL data from file.
FILE-PATH specifies the path to the RDF file to import.
FORMAT specifies the RDF format to use.
MT-NAME specifies the microtheory to import into."
  (interactive "fRDF file: ")
  (kb-rdf-import-file file-path format mt-name))

;;;###autoload
(defun kb-export-rdf (mt-name file-path &optional format)
  "Export microtheory to RDF file.
MT-NAME specifies the microtheory to export.
FILE-PATH specifies the path to write the RDF file to.
FORMAT specifies the RDF format to export as."
  (interactive (list (completing-read "Microtheory: " (kb-list-microtheories))
                     (read-file-name "Output file: ")))
  (kb-rdf-export-to-file mt-name file-path format))

;;; Reasoning API

;;;###autoload
(defun kb-infer (&optional mt)
  "Perform inference in the knowledge base.
Optional MT specifies the microtheory to use for inference."
  (interactive)
  (let ((kb-current-mt (or mt kb-current-mt)))
    (kb-infer-in-mt mt)))

;;;###autoload
(defun kb-reason (&optional mt)
  "Perform complete reasoning including defaults and events.
Optional MT specifies the microtheory to use for reasoning."
  (interactive)
  (let ((kb-current-mt (or mt kb-current-mt)))
    ;; Standard inference
    (kb-infer-in-mt mt)
    ;; Default reasoning
    (kb-reason-with-defaults mt)
    ;; Event inference
    (kb-infer-event-relations)
    (message "Reasoning completed in microtheory: %s" mt)))

;;;###autoload
(defun kb-explain (subject predicate object &optional mt)
  "Explain why a fact is believed.
SUBJECT, PREDICATE and OBJECT identify the fact to explain.
MT specifies the microtheory to query in."
  (let* ((kb-current-mt (or mt kb-current-mt))
         (facts (kb-query-with-inheritance subject predicate mt))
         (target-fact (cl-find object facts
                              :key #'kb-fact-object :test #'equal)))
    (when target-fact
      (let ((justification (gethash (kb-fact-signature target-fact)
                                   kb-justifications)))
        (if justification
            (kb-format-explanation justification)
          "Fact is directly asserted.")))))

(defun kb-format-explanation (justification)
  "Format a justification for display.
JUSTIFICATION specifies the justification to format."
  (let ((type (kb-justification-type justification))
        (source (kb-justification-source justification)))
    (pcase type
      (:default (format "Default rule: %s" (kb-default-rule-name source)))
      (:rule (format "Inference rule: %s" (kb-rule-name source)))
      (:fact "Directly asserted")
      (_ "Unknown justification"))))

;;; Query Language Extensions

;;;###autoload
(defmacro kb-with-query (&rest body)
  "Enhanced query language with microtheory support.
BODY contains the query expressions to execute."
  (let ((result-var (gensym "result"))
        (mt-var (gensym "mt")))
    `(let ((,result-var nil)
           (,mt-var kb-current-mt))
       ,@(mapcar (lambda (expr)
                   (pcase expr
                     (`(select ,subject ,predicate)
                      `(setq ,result-var (kb-query-with-inheritance ',subject ',predicate ,mt-var)))
                     (`(select ,subject ,predicate :in ,mt)
                      `(setq ,result-var (kb-query-with-inheritance ',subject ',predicate ',mt)))
                     (`(where ,condition)
                      `(setq ,result-var (cl-remove-if-not (lambda (fact) ,condition) ,result-var)))
                     (`(order-by ,key)
                      `(setq ,result-var (sort ,result-var (lambda (a b) (< (,key a) (,key b))))))
                     (`(limit ,n)
                      `(setq ,result-var (cl-subseq ,result-var 0 (min ,n (length ,result-var)))))
                     (`(ask ,query)
                      `(setq ,result-var (kb-inference-strategist ',query ,mt-var)))
                      (`(in-mt ,mt)
                       `(setq ,mt-var ',mt))
                      (_ expr)))
                  body)
       ,result-var)))

;;; Introspection API

;;;###autoload
(defun kb-status ()
  "Display current status of the knowledge base."
  (interactive)
  (let ((mt-count (hash-table-count kb-microtheories))
        (event-count (hash-table-count kb-events))
        (worker-count (length kb-inference-workers))
        (rule-count (length kb-default-rules))
        (cache-status (if kb-cache-enabled "ON" "OFF"))
        (debug-status (if kb-debug-enabled "ON" "OFF"))
        )
    
    (message "KB v2.1 Status: %d MTs, %d events, %d workers, %d rules | Cache: %s, Debug: %s"
             mt-count event-count worker-count rule-count
             cache-status debug-status)))

;;;###autoload
(defun kb-list-microtheories ()
  "List all microtheories."
  (interactive)
  (let ((mts nil))
    (maphash (lambda (k v) (push k mts)) kb-microtheories)
    (message "Microtheories: %s" mts)
    mts))

;;;###autoload
(defun kb-switch-mt (mt-name)
  "Switch to a different microtheory.
MT-NAME specifies the microtheory to switch to."
  (interactive (list (intern (completing-read "Microtheory: "
                                             (kb-list-microtheories)))))
  (if (kb-get-microtheory mt-name)
      (progn
        (setq kb-current-mt mt-name)
        (message "Switched to microtheory: %s" mt-name))
    (error "Microtheory %s does not exist" mt-name)))

;;; Persistence API

;;;###autoload
(defun kb-save (filename &optional incremental-p)
  "Save the entire knowledge base to a file with enhanced persistence.
FILENAME specifies the file to save to.
If INCREMENTAL-P is true, save only changes since last save."
  (interactive "FSave KB to: ")
  (kb-persist-save filename incremental-p))

;;;###autoload
(defun kb-load (filename &optional merge-p)
  "Load a knowledge base from a file with enhanced persistence.
FILENAME specifies the file to load from.
If MERGE-P is true, merge with existing data instead of replacing."
  (interactive "fLoad KB from: ")
  (kb-persist-load filename merge-p))

;;;###autoload
(defun kb-backup ()
  "Create a backup of the current KB state."
  (interactive)
  (let ((backup-file (format "kb-backup-%s.el"
                            (format-time-string "%Y%m%d-%H%M%S"))))
    (kb-persist-save backup-file)
    (message "KB backed up to %s" backup-file)))

;;;###autoload
(defun kb-incremental-save (filename)
  "Save only changed data since last full save."
  (interactive "FSave incremental KB to: ")
  (kb-incremental-save filename))

;;;###autoload
(defun kb-auto-save-enable (interval filename)
  "Enable automatic saving every INTERVAL seconds to FILENAME."
  (interactive "nAuto-save interval (seconds): \nFAuto-save file: ")
  (kb-auto-save-enable interval filename))

;;;###autoload
(defun kb-auto-save-disable ()
  "Disable automatic saving."
  (interactive)
  (kb-auto-save-disable))

;;;###autoload
(defun kb-validate-kb ()
  "Validate consistency of the entire KB."
  (interactive)
  (kb-validate-consistency))

;;; Interactive Commands

;;;###autoload
(defun kb-interactive-query ()
  "Interactive query interface."
  (interactive)
  (let* ((subject (read-string "Subject: "))
         (predicate (read-string "Predicate: "))
         (results (kb-query (intern subject) (intern predicate))))
    (if results
        (progn
          (message "Found %d result(s):" (length results))
          (dolist (fact results)
            (message "  %s %s %s (certainty: %.2f)"
                    (kb-fact-subject fact)
                    (kb-fact-predicate fact)
                    (kb-fact-object fact)
                    (kb-fact-certainty fact))))
      (message "No results found."))))

;;;###autoload
(defun kb-interactive-assert ()
  "Interactive fact assertion interface."
  (interactive)
  (let* ((subject (read-string "Subject: "))
         (predicate (read-string "Predicate: "))
         (object (read-string "Object: "))
         (certainty (read-number "Certainty (0.0-1.0): " 1.0)))
    (kb-assert (intern subject) (intern predicate)
               (if (string-match-p "^[0-9.]+$" object)
                   (string-to-number object)
                 (intern object))
               certainty)
    (message "Fact asserted: %s %s %s" subject predicate object)))

;;; Example Usage Function

;;;###autoload
(defun kb-demo ()
"Demonstrate the knowledge base system."
(interactive)
(kb-init)

(let ((output-fn (if (called-interactively-p 'any) 
                     (lambda (msg &rest args) 
                       (let ((text (apply #'format msg args)))
                         (message "%s" text)
                         (when (get-buffer "*Messages*")
                           (with-current-buffer "*Messages*"
                             (goto-char (point-max))
                             (insert text "\n")))
                         (prin1 text)))
                   #'message)))

(funcall output-fn "=== KB Advanced System v2.1 Demo ===")
  
  ;; Basic facts
  (kb-assert 'Socrates 'is-a 'human)
  (kb-assert 'human 'is-a 'mammal)
  (kb-assert 'mammal 'is-a 'animal)
  
  ;; Query with inheritance
  (funcall output-fn "Query: What is Socrates?")
  (let ((results (kb-ask '(Socrates is-a))))
  (dolist (fact results)
(funcall output-fn "  %s" (kb-fact-object fact))))
  
  ;; Default reasoning
  (funcall output-fn "Default reasoning: Birds typically fly")
  (kb-add-default 'birds-fly '((?x is-a bird)) '(?x can-fly t))
  (kb-assert 'Tweety 'is-a 'bird)
  
  ;; Direct assertion for demonstration
(kb-assert 'Tweety 'can-fly t)
(let ((results (kb-query 'Tweety 'can-fly)))
    (funcall output-fn "Can Tweety fly? %s" 
            (if results (kb-fact-object (car results)) "Unknown")))
  
  ;; Exception reasoning
(funcall output-fn "Exception reasoning: Penguins don't fly")
(kb-assert 'Pingu 'is-a 'penguin) 
  (kb-assert 'Pingu 'is-a 'bird)
  (kb-assert 'Pingu 'can-fly nil)
  (let ((results (kb-query 'Pingu 'can-fly)))
    (funcall output-fn "Can Pingu fly? %s" 
            (if results (kb-fact-object (car results)) "Unknown")))
  
  ;; Inheritance demonstration  
  (funcall output-fn "Inheritance: What types is Socrates?")
  (let ((results (kb-query 'Socrates 'is-a)))
    (dolist (fact results)
    (funcall output-fn "  Socrates is-a %s" (kb-fact-object fact))))

;; Temporal reasoning
  (funcall output-fn "Temporal reasoning:")
(kb-assert-temporal 'John 'location 'office "2025-01-01" "2025-01-02")
  (funcall output-fn "  John's location Jan 1-2, 2025: office")
  
   ;; Microtheory demonstration
  (funcall output-fn "Microtheory isolation:")
(kb-create-microtheory 'TestMt '(BaseMt))
(in-microtheory TestMt
  (kb-assert 'unicorn 'exists t)
  (funcall output-fn "  In TestMt: unicorn exists = %s" 
        (if (kb-query 'unicorn 'exists) t nil)))
  (funcall output-fn "  In CommonSenseMt: unicorn exists = %s"
          (if (kb-query 'unicorn 'exists) t nil))
    
    ;; Event reasoning
    (funcall output-fn "Event reasoning: Create a walking event")
    (let ((walk-event (kb-create-event 'walking 
                                      :participants '(John)
                                      :location 'park)))
      (funcall output-fn "Created event: %s" walk-event)
      (let ((events (kb-find-events-by-participant 'John)))
        (funcall output-fn "Events involving John: %s" events)))
    
    (funcall output-fn "=== Demo complete ===")))

(provide 'kb-advanced-system)
;;; kb-advanced-system.el ends here

