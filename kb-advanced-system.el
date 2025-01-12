;;; kb-advanced-system.el --- Unified Advanced Knowledge Base System

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
(require 'kb-database)

;;; High-level API

;;;###autoload
(defun kb-init (&optional base-mt)
  "Initialize the knowledge base system."
  (interactive)
  
  ;; Initialize microtheories
  (unless (kb-get-microtheory 'BaseMt)
    (kb-create-microtheory 'BaseMt))
  (kb-create-microtheory 'CommonSenseMt 'BaseMt)
  (kb-create-microtheory 'TemporalMt 'BaseMt)
  (kb-create-microtheory 'EventMt 'BaseMt)
  
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
  "Assert a fact in the knowledge base."
  (interactive "sSubject: \nsPredicate: \nsObject: ")
  (let ((kb-current-mt (or mt kb-current-mt)))
    (kb-add-fact subject predicate object certainty temporal-info)))

;;;###autoload
(defun kb-assert-temporal (subject predicate object valid-from valid-to &optional certainty mt)
  "Assert a temporally bounded fact."
  (let ((kb-current-mt (or mt kb-current-mt)))
    (kb-add-temporal-fact subject predicate object valid-from valid-to certainty)))

;;;###autoload
(defun kb-retract (subject predicate object &optional mt)
  "Retract a fact from the knowledge base."
  (let* ((kb-current-mt (or mt kb-current-mt))
         (facts (kb-query-with-inheritance subject predicate mt)))
    (dolist (fact facts)
      (when (equal (kb-fact-object fact) object)
        (kb-remove-fact fact)))))

;;;###autoload
(defun kb-query (subject predicate &optional mt)
  "Query the knowledge base."
  (interactive "sSubject: \nsPredicate: ")
  (let ((kb-current-mt (or mt kb-current-mt)))
    (kb-query-with-inheritance subject predicate mt)))

;;;###autoload
(defun kb-ask (query &optional mt timeout)
  "Ask a complex query using the inference engine."
  (let ((kb-current-mt (or mt kb-current-mt)))
    (kb-inference-strategist query mt timeout)))

;;; Rule Management API

;;;###autoload
(defun kb-add-rule (name premises conclusion &optional priority temporal-p mt)
  "Add an inference rule."
  (let ((kb-current-mt (or mt kb-current-mt)))
    (kb-add-rule name premises conclusion priority temporal-p)))

;;;###autoload
(defun kb-add-default (name premises conclusion &optional exceptions strength specificity mt)
  "Add a default rule with possible exceptions."
  (let ((kb-current-mt (or mt kb-current-mt)))
    (kb-add-default-rule name premises conclusion exceptions strength specificity)))

;;;###autoload
(defun kb-add-exception (name rule-name conditions &optional alternative priority mt)
  "Add an exception to a default rule."
  (let ((kb-current-mt (or mt kb-current-mt)))
    (kb-add-exception name rule-name conditions alternative priority)))

;;; Event Management API

;;;###autoload
(defun kb-create-event (type &rest properties)
  "Create a new event."
  (apply #'kb-create-event type properties))

;;;###autoload
(defun kb-define-process (name &rest properties)
  "Define a new process type."
  (apply #'kb-define-process name properties))

;;;###autoload
(defun kb-relate-events (relation source target &optional strength)
  "Add a relation between events."
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
  "Query with debugging trace enabled."
  (let ((orig-debug kb-debug-enabled))
    (unless orig-debug (kb-debug-enable))
    (unwind-protect
        (kb-debug-query subject predicate mt)
      (unless orig-debug (kb-debug-disable)))))

;;;###autoload  
(defun kb-validate (mt-name)
  "Validate consistency of a microtheory."
  (interactive (list (completing-read "Microtheory: " (kb-list-microtheories))))
  (kb-validate-microtheory mt-name))

;;; Caching API

;;;###autoload
(defun kb-cache-on (&optional max-size ttl)
  "Enable query caching."
  (interactive)
  (kb-cache-enable max-size ttl))

;;;###autoload
(defun kb-cache-off ()
  "Disable query caching."
  (interactive)
  (kb-cache-disable))

;;;###autoload
(defun kb-cached-query (subject predicate &optional mt)
  "Query with caching enabled."
  (kb-cached-query subject predicate mt))

;;; Testing API

;;;###autoload
(defun kb-run-tests (&optional suite-name)
  "Run knowledge base tests."
  (interactive)
  (if suite-name
      (kb-run-test-suite suite-name)
    (kb-run-all-tests)))

;;; Database API

;;;###autoload
(defun kb-db-connect (db-path)
  "Connect to SQLite database."
  (interactive "fDatabase file: ")
  (kb-db-connect-sqlite db-path))

;;;###autoload
(defun kb-save-to-db (mt-name)
  "Save microtheory to database."
  (interactive (list (completing-read "Microtheory: " (kb-list-microtheories))))
  (kb-db-save-microtheory mt-name))

;;;###autoload
(defun kb-load-from-db (mt-name)
  "Load microtheory from database."
  (interactive (list (completing-read "Microtheory: " (kb-db-list-microtheories))))
  (kb-db-load-microtheory mt-name))

;;; RDF API

;;;###autoload
(defun kb-import-rdf (file-path &optional format mt-name)
  "Import RDF/OWL data from file."
  (interactive "fRDF file: ")
  (kb-rdf-import-file file-path format mt-name))

;;;###autoload
(defun kb-export-rdf (mt-name file-path &optional format)
  "Export microtheory to RDF file."
  (interactive (list (completing-read "Microtheory: " (kb-list-microtheories))
                     (read-file-name "Output file: ")))
  (kb-rdf-export-to-file mt-name file-path format))

;;; Reasoning API

;;;###autoload
(defun kb-infer (&optional mt)
  "Perform inference in the knowledge base."
  (interactive)
  (let ((kb-current-mt (or mt kb-current-mt)))
    (kb-infer-in-mt mt)))

;;;###autoload
(defun kb-reason (&optional mt)
  "Perform complete reasoning including defaults and events."
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
  "Explain why a fact is believed."
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
  "Format a justification for display."
  (let ((type (kb-justification-type justification))
        (source (kb-justification-source justification)))
    (case type
      (:default (format "Default rule: %s" (kb-default-rule-name source)))
      (:rule (format "Inference rule: %s" (kb-rule-name source)))
      (:fact "Directly asserted")
      (t "Unknown justification"))))

;;; Query Language Extensions

;;;###autoload
(defmacro kb-with-query (&rest body)
  "Enhanced query language with microtheory support."
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
                     (t expr)))
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
        (db-status (if kb-db-connection "CONNECTED" "DISCONNECTED")))
    
    (message "KB v2.1 Status: %d MTs, %d events, %d workers, %d rules | Cache: %s, Debug: %s, DB: %s"
             mt-count event-count worker-count rule-count 
             cache-status debug-status db-status)))

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
  "Switch to a different microtheory."
  (interactive (list (intern (completing-read "Microtheory: " 
                                             (kb-list-microtheories)))))
  (if (kb-get-microtheory mt-name)
      (progn
        (setq kb-current-mt mt-name)
        (message "Switched to microtheory: %s" mt-name))
    (error "Microtheory %s does not exist" mt-name)))

;;; Import/Export

;;;###autoload
(defun kb-save (filename)
  "Save the entire knowledge base to a file."
  (interactive "FSave KB to: ")
  (with-temp-file filename
    (prin1 (list :microtheories kb-microtheories
                 :events kb-events
                 :processes kb-processes
                 :default-rules kb-default-rules
                 :exceptions kb-exceptions
                 :justifications kb-justifications)
           (current-buffer))))

;;;###autoload
(defun kb-load (filename)
  "Load a knowledge base from a file."
  (interactive "fLoad KB from: ")
  (when (file-exists-p filename)
    (let ((data (with-temp-buffer
                  (insert-file-contents filename)
                  (read (current-buffer)))))
      (setq kb-microtheories (plist-get data :microtheories)
            kb-events (plist-get data :events)
            kb-processes (plist-get data :processes)
            kb-default-rules (plist-get data :default-rules)
            kb-exceptions (plist-get data :exceptions)
            kb-justifications (plist-get data :justifications))
      (message "Knowledge base loaded from %s" filename))))

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
  
  (message "=== KB Advanced System v2.1 Demo ===")
  
  ;; Basic facts
  (kb-assert 'Socrates 'is-a 'human)
  (kb-assert 'human 'is-a 'mammal)
  (kb-assert 'mammal 'is-a 'animal)
  
  ;; Query with inheritance
  (message "Query: What is Socrates?")
  (let ((results (kb-ask '(Socrates is-a))))
    (dolist (fact results)
      (message "  %s" (kb-fact-object fact))))
  
  ;; Default reasoning
  (message "Default reasoning: Birds typically fly")
  (in-microtheory CommonSenseMt
    (kb-add-default 'birds-fly '((?x is-a bird)) '(?x can-fly t))
    (kb-assert 'Tweety 'is-a 'bird)
    (kb-reason)
    (let ((results (kb-query 'Tweety 'can-fly)))
      (message "Can Tweety fly? %s" 
              (if results (kb-fact-object (car results)) "Unknown"))))
  
  ;; Event reasoning
  (message "Event reasoning: Create a walking event")
  (let ((walk-event (kb-create-event 'walking 
                                    :participants '(John)
                                    :location 'park)))
    (message "Created event: %s" walk-event)
    (let ((events (kb-find-events-by-participant 'John)))
      (message "Events involving John: %s" events)))
  
  (message "=== Demo complete ==="))

(provide 'kb-advanced-system)
;;; kb-advanced-system.el ends here
