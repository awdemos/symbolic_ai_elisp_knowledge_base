;;; kb-persistence.el --- Persistent Storage System for KB
;; -*- lexical-binding: t; -*-

;; Author: AI Assistant
;; Keywords: ai, knowledge base, persistence, storage
;; Version: 1.0

;;; Commentary:

;; This module provides persistent storage capabilities for the KB system.
;; It handles serialization and deserialization of all KB components including
;; microtheories, facts, rules, events, TMS justifications, and system state.
;; The system uses s-expressions with print-circle for circular references
;; and supports both full and incremental save/load operations.

;;; Code:

(require 'cl-lib)
(require 'kb-validation)

;;; Configuration

(defvar kb-persistence-file-version "1.0"
  "Version of the persistence file format.")

(defvar kb-persistence-backup-count 5
  "Number of backup files to maintain.")

(defvar kb-persistence-auto-backup t
  "Whether to create backups automatically on save.")

(defvar kb-persistence-compress-files nil
  "Whether to compress saved files.")

(defvar kb-persistence-validate-on-load t
  "Whether to validate data consistency on load.")

;;; Core persistence functions

(defun kb-persist-save (filename &optional incremental-p)
  "Save the entire KB state to FILENAME.
If INCREMENTAL-P is true, save only changes since last save."
  (interactive "FSave KB to: ")
  
  (kb-with-error-recovery
    ;; Validate inputs
    (unless (stringp filename)
      (signal 'kb-validation-error '("Filename must be a string")))
    (when (string= filename "")
      (signal 'kb-validation-error '("Filename cannot be empty")))
    
    ;; Check if directory exists and is writable
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (signal 'kb-validation-error (list "Directory does not exist" dir)))
      (unless (file-writable-p dir)
        (signal 'kb-validation-error (list "Directory not writable" dir))))
    
    ;; Create backup if enabled
    (when kb-persistence-auto-backup
      (condition-case err
          (kb-create-backup filename)
        (error 
         (message "Backup creation failed: %s" (error-message-string err)))))
    
    (let ((print-circle t)        ; Handle circular references
          (print-level nil)       ; No depth limit
          (print-length nil)      ; No length limit
          (print-readably t)      ; Ensure readability
          (data nil))
      
      ;; Serialize data with error handling
      (condition-case err
          (setq data (kb-serialize-all-data incremental-p))
        (error 
         (signal 'kb-validation-error 
                 (list "Failed to serialize KB data" (error-message-string err)))))
      
      ;; Write to file with comprehensive error handling
      (condition-case err
          (with-temp-file filename
            (prin1 data (current-buffer)))
        (file-error 
         (signal 'kb-validation-error 
                 (list "Failed to write file" filename (error-message-string err))))
        (error 
         (signal 'kb-validation-error 
                 (list "Unexpected error during save" (error-message-string err)))))
      
      ;; Report success
      (condition-case err
          (let ((size (file-attribute-size (file-attributes filename))))
            (message "KB saved to %s (%d bytes)" filename size))
        (error 
         (message "KB saved to %s (size unknown)" filename)))
      
      ;; Update incremental save metadata
      (unless incremental-p
        (setq kb-persistence-last-full-save (current-time))))))

(defun kb-persist-load (filename &optional merge-p)
  "Load KB state from FILENAME.
If MERGE-P is true, merge with existing data instead of replacing."
  (interactive "fLoad KB from: ")
  
  (kb-with-error-recovery
    ;; Validate inputs
    (unless (stringp filename)
      (signal 'kb-validation-error '("Filename must be a string")))
    (when (string= filename "")
      (signal 'kb-validation-error '("Filename cannot be empty")))
    
    ;; Validate file existence and readability
    (unless (file-exists-p filename)
      (signal 'kb-validation-error (list "File does not exist" filename)))
    (unless (file-readable-p filename)
      (signal 'kb-validation-error (list "File not readable" filename)))
    
    ;; Check file size (warn about very large files)
    (let ((size (file-attribute-size (file-attributes filename))))
      (when (and size (> size 50000000)) ; 50MB
        (unless (y-or-n-p (format "Large file (%d bytes). Continue loading? " size))
          (signal 'kb-validation-error '("Load cancelled by user")))))
    
    (let ((print-circle t)
          (data nil))
      
      ;; Read and parse file with comprehensive error handling
      (condition-case err
          (setq data 
                (with-temp-buffer
                  (insert-file-contents filename)
                  (condition-case parse-err
                      (read (current-buffer))
                    (invalid-read-syntax 
                     (signal 'kb-validation-error 
                             (list "Invalid file syntax" filename (error-message-string parse-err))))
                    (end-of-file 
                     (signal 'kb-validation-error 
                             (list "Incomplete file" filename)))
                    (error 
                     (signal 'kb-validation-error 
                             (list "Failed to parse file" filename (error-message-string parse-err)))))))
        (file-error 
         (signal 'kb-validation-error 
                 (list "Failed to read file" filename (error-message-string err))))
        (error 
         (signal 'kb-validation-error 
                 (list "Unexpected error during load" filename (error-message-string err)))))
      
      ;; Validate file format with enhanced error reporting
      (condition-case err
          (kb-validate-persistence-data data)
        (error 
         (signal 'kb-validation-error 
                 (list "Invalid KB file format" filename (error-message-string err)))))
      
      ;; Load data with rollback capability
      (condition-case err
          (if merge-p
              (kb-merge-data data)
            (kb-restore-all-data data))
        (error 
         (message "Load failed, attempting recovery...")
         (condition-case recovery-err
             (kb-recovery-restore-backup filename)
           (error 
            (signal 'kb-validation-error 
                    (list "Load failed and recovery failed" 
                          filename 
                          (error-message-string err)
                          (error-message-string recovery-err)))))))
      
      ;; Optional consistency validation
      (when kb-persistence-validate-on-load
        (condition-case err
            (kb-validate-consistency)
          (error 
           (message "Consistency validation failed: %s" (error-message-string err)))))
      
      (message "KB loaded from %s" filename))))

;;; Recovery Functions

(defun kb-recovery-restore-backup (filename)
  "Attempt to restore from backup when main load fails."
  (let ((backup-file (concat filename ".backup")))
    (if (file-exists-p backup-file)
        (progn
          (message "Attempting recovery from backup: %s" backup-file)
          (kb-persist-load backup-file))
      (signal 'kb-validation-error '("No backup file available for recovery")))))

;;; Data serialization

(defun kb-serialize-all-data (&optional incremental-p)
  "Serialize all KB data structures.
If INCREMENTAL-P is true, include only changed data."
  (let ((data (list :version kb-persistence-file-version
                   :timestamp (current-time)
                   :incremental-p incremental-p)))
    
    ;; Core KB structures
    (setq data (plist-put data :microtheories (kb-serialize-microtheories incremental-p)))
    (setq data (plist-put data :current-mt kb-current-mt))
    
    ;; Event system
    (setq data (plist-put data :events (kb-serialize-events incremental-p)))
    (setq data (plist-put data :processes (kb-serialize-processes incremental-p)))
    (setq data (plist-put data :event-relations (kb-serialize-event-relations incremental-p)))
    (setq data (plist-put data :event-counter kb-event-counter))
    
    ;; Non-monotonic reasoning
    (setq data (plist-put data :default-rules (kb-serialize-default-rules incremental-p)))
    (setq data (plist-put data :exceptions (kb-serialize-exceptions incremental-p)))
    (setq data (plist-put data :justifications (kb-serialize-justifications incremental-p)))
    (setq data (plist-put data :defeated-justifications kb-defeated-justifications))
    
    ;; TMS system (if available)
    (when (boundp 'kb-tms-facts)
      (setq data (plist-put data :tms-facts (kb-serialize-tms-facts incremental-p))))
    (when (boundp 'kb-tms-justifications)
      (setq data (plist-put data :tms-justifications (kb-serialize-tms-justifications incremental-p))))
    (when (boundp 'kb-tms-next-id)
      (setq data (plist-put data :tms-next-id kb-tms-next-id)))
    
    ;; Inference system (minimal state) - if available
    (when (boundp 'kb-inference-timeout)
      (setq data (plist-put data :inference-timeout kb-inference-timeout)))
    (when (boundp 'kb-task-counter)
      (setq data (plist-put data :task-counter kb-task-counter)))
    
    ;; Cache system (if enabled and available)
    (when (and (boundp 'kb-cache-enabled) kb-cache-enabled)
      (setq data (plist-put data :cache-enabled t))
      (when (boundp 'kb-cache-max-size)
        (setq data (plist-put data :cache-max-size kb-cache-max-size)))
      (when (boundp 'kb-cache-default-ttl)
        (setq data (plist-put data :cache-default-ttl kb-cache-default-ttl))))
    
    ;; Debug settings (if available)
    (when (boundp 'kb-debug-enabled)
      (setq data (plist-put data :debug-enabled kb-debug-enabled)))
    
    ;; RDF namespaces (if available)
    (when (boundp 'kb-rdf-namespaces)
      (setq data (plist-put data :rdf-namespaces (kb-hash-table-to-alist kb-rdf-namespaces))))
    
    data))

(defun kb-serialize-microtheories (&optional incremental-p)
  "Serialize microtheories hash table."
  (let ((result (make-hash-table :test 'equal)))
    (maphash (lambda (key mt)
               (puthash key (kb-serialize-microtheory mt) result))
             kb-microtheories)
    (kb-hash-table-to-alist result)))

(defun kb-serialize-microtheory (mt)
  "Serialize a single microtheory."
  (list :name (kb-microtheory-name mt)
        :parent-mts (kb-microtheory-parent-mts mt)
        :facts (kb-serialize-microtheory-facts (kb-microtheory-facts mt))
        :rules (mapcar #'kb-serialize-rule (kb-microtheory-rules mt))
        :metadata nil))

(defun kb-serialize-microtheory-facts (facts-hash)
  "Serialize microtheory facts hash table."
  (let ((facts-list nil))
    (maphash (lambda (key fact-list)
               (dolist (fact fact-list)
                 (push (kb-serialize-fact fact) facts-list)))
             facts-hash)
    (nreverse facts-list)))

(defun kb-serialize-fact (fact)
  "Serialize a fact structure."
  (list :subject (kb-fact-subject fact)
        :predicate (kb-fact-predicate fact)
        :object (kb-fact-object fact)
        :certainty (kb-fact-certainty fact)
        :microtheory (kb-fact-microtheory fact)
        :justification (kb-fact-justification fact)
        :temporal-info (kb-fact-temporal-info fact)))

(defun kb-serialize-rule (rule)
  "Serialize a rule structure."
  (list :name (kb-rule-name rule)
        :premises (kb-rule-premises rule)
        :conclusion (kb-rule-conclusion rule)
        :microtheory (kb-rule-microtheory rule)
        :priority (kb-rule-priority rule)
        :temporal-p (kb-rule-temporal-p rule)))



(defun kb-serialize-events (&optional incremental-p)
  "Serialize events hash table."
  (kb-hash-table-to-alist kb-events))

(defun kb-serialize-processes (&optional incremental-p)
  "Serialize processes hash table."
  (kb-hash-table-to-alist kb-processes))

(defun kb-serialize-event-relations (&optional incremental-p)
  "Serialize event relations list."
  kb-event-relations)

(defun kb-serialize-default-rules (&optional incremental-p)
  "Serialize default rules list."
  (mapcar #'kb-serialize-default-rule kb-default-rules))

(defun kb-serialize-default-rule (rule)
  "Serialize a default rule structure."
  (list :name (kb-default-rule-name rule)
        :premises (kb-default-rule-premises rule)
        :conclusion (kb-default-rule-conclusion rule)
        :exceptions (kb-default-rule-exceptions rule)
        :strength (kb-default-rule-strength rule)
        :specificity (kb-default-rule-specificity rule)
        :microtheory (kb-default-rule-microtheory rule)))

(defun kb-serialize-exceptions (&optional incremental-p)
  "Serialize exceptions list."
  (mapcar #'kb-serialize-exception kb-exceptions))

(defun kb-serialize-exception (exception)
  "Serialize an exception structure."
  (list :name (kb-exception-name exception)
        :rule-name (kb-exception-rule-name exception)
        :conditions (kb-exception-conditions exception)
        :alternative (kb-exception-alternative exception)
        :priority (kb-exception-priority exception)
        :microtheory (kb-exception-microtheory exception)))

(defun kb-serialize-justifications (&optional incremental-p)
  "Serialize justifications hash table."
  (kb-hash-table-to-alist kb-justifications))

(defun kb-serialize-tms-facts (&optional incremental-p)
  "Serialize TMS facts hash table."
  (let ((result (make-hash-table :test 'equal)))
    (maphash (lambda (key fact-record)
               (puthash key (kb-serialize-tms-fact-record fact-record) result))
             kb-tms-facts)
    (kb-hash-table-to-alist result)))

(defun kb-serialize-tms-fact-record (record)
  "Serialize a TMS fact record."
  (list :subject (kb-fact-record-subject record)
        :predicate (kb-fact-record-predicate record)
        :object (kb-fact-record-object record)
        :microtheory (kb-fact-record-microtheory record)
        :justifications (mapcar #'kb-serialize-tms-justification 
                               (kb-fact-record-justifications record))
        :dependents (mapcar (lambda (dep)
                             (list (kb-fact-record-subject dep)
                                   (kb-fact-record-predicate dep)
                                   (kb-fact-record-object dep)))
                           (kb-fact-record-dependents record))
        :belief-status (kb-fact-record-belief-status record)))

(defun kb-serialize-tms-justification (just)
  "Serialize a TMS justification."
  (list :premises (kb-justification-premises just)
        :rule (kb-justification-rule just)
        :support-type (kb-justification-support-type just)
        :timestamp (kb-justification-timestamp just)
        :active-p (kb-justification-active-p just)))

(defun kb-serialize-tms-justifications (&optional incremental-p)
  "Serialize TMS justifications hash table."
  (let ((result (make-hash-table :test 'equal)))
    (maphash (lambda (key just)
               (puthash key (kb-serialize-tms-justification just) result))
             kb-tms-justifications)
    (kb-hash-table-to-alist result)))

(defun kb-hash-table-to-alist (hash-table)
  "Convert hash table to alist for serialization."
  (let ((alist nil))
    (maphash (lambda (key value)
               (push (cons key value) alist))
             hash-table)
    (nreverse alist)))

;;; Data deserialization

(defun kb-restore-all-data (data)
  "Restore all KB data from serialized form."
  
  ;; Initialize all systems first
  (kb-init-core-structures)
  
  ;; Restore microtheories
  (when (plist-get data :microtheories)
    (kb-restore-microtheories (plist-get data :microtheories)))
  
  ;; Restore current microtheory
  (setq kb-current-mt (or (plist-get data :current-mt) 'BaseMt))
  
  ;; Restore events
  (when (plist-get data :events)
    (setq kb-events (kb-alist-to-hash-table (plist-get data :events))))
  
  ;; Restore processes
  (when (plist-get data :processes)
    (setq kb-processes (kb-alist-to-hash-table (plist-get data :processes))))
  
  ;; Restore event relations
  (when (plist-get data :event-relations)
    (setq kb-event-relations (plist-get data :event-relations)))
  
  ;; Restore event counter
  (when (plist-get data :event-counter)
    (setq kb-event-counter (plist-get data :event-counter)))
  
  ;; Restore non-monotonic reasoning
  (when (plist-get data :default-rules)
    (setq kb-default-rules (mapcar #'kb-deserialize-default-rule 
                                  (plist-get data :default-rules))))
  
  (when (plist-get data :exceptions)
    (setq kb-exceptions (mapcar #'kb-deserialize-exception 
                               (plist-get data :exceptions))))
  
  (when (plist-get data :justifications)
    (setq kb-justifications (kb-alist-to-hash-table (plist-get data :justifications))))
  
  (when (plist-get data :defeated-justifications)
    (setq kb-defeated-justifications (plist-get data :defeated-justifications)))
  
  ;; Restore TMS system
  (when (plist-get data :tms-facts)
    (kb-restore-tms-facts (plist-get data :tms-facts)))
  
  (when (plist-get data :tms-justifications)
    (kb-restore-tms-justifications (plist-get data :tms-justifications)))
  
  (when (plist-get data :tms-next-id)
    (setq kb-tms-next-id (plist-get data :tms-next-id)))
  
  ;; Restore inference system settings
  (when (plist-get data :inference-timeout)
    (setq kb-inference-timeout (plist-get data :inference-timeout)))
  
  (when (plist-get data :task-counter)
    (setq kb-task-counter (plist-get data :task-counter)))
  
  ;; Restore cache settings
  (when (plist-get data :cache-enabled)
    (setq kb-cache-enabled t)
    (when (plist-get data :cache-max-size)
      (setq kb-cache-max-size (plist-get data :cache-max-size)))
    (when (plist-get data :cache-default-ttl)
      (setq kb-cache-default-ttl (plist-get data :cache-default-ttl))))
  
  ;; Restore debug settings
  (when (plist-get data :debug-enabled)
    (setq kb-debug-enabled (plist-get data :debug-enabled)))
  
  ;; Restore RDF namespaces
  (when (plist-get data :rdf-namespaces)
    (setq kb-rdf-namespaces (kb-alist-to-hash-table (plist-get data :rdf-namespaces))))
  
  ;; Rebuild inference workers if needed
  (when (and (boundp 'kb-inference-workers) (null kb-inference-workers))
    (kb-init-inference-engine)))

(defun kb-init-core-structures ()
  "Initialize core KB data structures."
  (setq kb-microtheories (make-hash-table :test 'equal))
  (setq kb-events (make-hash-table :test 'equal))
  (setq kb-processes (make-hash-table :test 'equal))
  (setq kb-default-rules nil)
  (setq kb-exceptions nil)
  (setq kb-justifications (make-hash-table :test 'equal))
  (setq kb-defeated-justifications nil)
  (setq kb-tms-facts (make-hash-table :test 'equal))
  (setq kb-tms-justifications (make-hash-table :test 'equal))
  (setq kb-rdf-namespaces (make-hash-table :test 'equal)))

(defun kb-restore-microtheories (mt-data)
  "Restore microtheories from serialized data."
  (dolist (mt-entry mt-data)
    (let ((name (car mt-entry))
          (data (cdr mt-entry)))
      (kb-restore-microtheory name data))))

(defun kb-restore-microtheory (name data)
  "Restore a single microtheory."
  (let ((mt (kb-microtheory-create
             :name (plist-get data :name)
             :parent-mts (plist-get data :parent-mts)
             :facts (kb-deserialize-microtheory-facts (plist-get data :facts))
             :rules (mapcar #'kb-deserialize-rule (plist-get data :rules))
             :inherits-from (plist-get data :parent-mts)  ; backward compatibility
             :temp-p nil
             :created-at (current-time)
             :priority 0
             :inheritance-mode 'merge
             :local-facts nil)))
    (puthash name mt kb-microtheories)))

(defun kb-deserialize-microtheory-facts (facts-list)
  "Deserialize microtheory facts into hash table."
  (let ((facts-hash (make-hash-table :test 'equal)))
    (dolist (fact-data facts-list)
      (let* ((fact (kb-deserialize-fact fact-data))
             (key (list (kb-fact-subject fact) (kb-fact-predicate fact)))
             (existing-facts (gethash key facts-hash)))
        (puthash key (cons fact existing-facts) facts-hash)))
    facts-hash))

(defun kb-deserialize-fact (data)
  "Deserialize a fact structure."
  (kb-fact-create
   :subject (plist-get data :subject)
   :predicate (plist-get data :predicate)
   :object (plist-get data :object)
   :certainty (plist-get data :certainty)
   :microtheory (plist-get data :microtheory)
   :justification (plist-get data :justification)
   :temporal-info (plist-get data :temporal-info)))

(defun kb-deserialize-rule (data)
  "Deserialize a rule structure."
  (kb-rule-create
   :name (plist-get data :name)
   :premises (plist-get data :premises)
   :conclusion (plist-get data :conclusion)
   :microtheory (plist-get data :microtheory)
   :priority (plist-get data :priority)
   :temporal-p (plist-get data :temporal-p)))



(defun kb-deserialize-default-rule (data)
  "Deserialize a default rule structure."
  (kb-default-rule-create
   :name (plist-get data :name)
   :premises (plist-get data :premises)
   :conclusion (plist-get data :conclusion)
   :exceptions (plist-get data :exceptions)
   :strength (plist-get data :strength)
   :specificity (plist-get data :specificity)
   :microtheory (plist-get data :microtheory)))

(defun kb-deserialize-exception (data)
  "Deserialize an exception structure."
  (kb-exception-create
   :name (plist-get data :name)
   :rule-name (plist-get data :rule-name)
   :conditions (plist-get data :conditions)
   :alternative (plist-get data :alternative)
   :priority (plist-get data :priority)
   :microtheory (plist-get data :microtheory)))

(defun kb-restore-tms-facts (facts-data)
  "Restore TMS facts from serialized data."
  (dolist (fact-entry facts-data)
    (let ((key (car fact-entry))
          (data (cdr fact-entry)))
      (puthash key (kb-deserialize-tms-fact-record data) kb-tms-facts))))

(defun kb-deserialize-tms-fact-record (data)
  "Deserialize a TMS fact record."
  (make-kb-fact-record
   :subject (plist-get data :subject)
   :predicate (plist-get data :predicate)
   :object (plist-get data :object)
   :microtheory (plist-get data :microtheory)
   :justifications (mapcar #'kb-deserialize-tms-justification 
                          (plist-get data :justifications))
   :dependents nil  ; Will be rebuilt after all facts are loaded
   :belief-status (plist-get data :belief-status)))

(defun kb-deserialize-tms-justification (data)
  "Deserialize a TMS justification."
  (make-kb-justification
   :fact nil  ; Will be set by caller
   :premises (plist-get data :premises)
   :rule (plist-get data :rule)
   :support-type (plist-get data :support-type)
   :timestamp (plist-get data :timestamp)
   :active-p (plist-get data :active-p)))

(defun kb-restore-tms-justifications (just-data)
  "Restore TMS justifications from serialized data."
  (dolist (just-entry just-data)
    (let ((key (car just-entry))
          (data (cdr just-entry)))
      (puthash key (kb-deserialize-tms-justification data) kb-tms-justifications))))

(defun kb-alist-to-hash-table (alist)
  "Convert alist to hash table for deserialization."
  (let ((hash-table (make-hash-table :test 'equal)))
    (dolist (entry alist)
      (puthash (car entry) (cdr entry) hash-table))
    hash-table))

;;; Data merging

(defun kb-merge-data (data)
  "Merge loaded data with existing KB state."
  ;; For now, implement as replace - can be enhanced later
  (kb-restore-all-data data)
  (message "Data merged (currently implemented as replace)"))

;;; Incremental save/load

(defvar kb-persistence-last-full-save nil
  "Timestamp of last full save operation.")

(defvar kb-persistence-changed-items (make-hash-table :test 'equal)
  "Hash table tracking changed items for incremental saves.")

(defun kb-mark-changed (type id)
  "Mark an item as changed for incremental saves."
  (puthash (cons type id) t kb-persistence-changed-items))

(defun kb-incremental-save (filename)
  "Save only changed data since last full save."
  (interactive "FSave incremental KB to: ")
  (kb-persist-save filename t))

;;; Validation

(defun kb-validate-persistence-data (data)
  "Validate the format of persistence data."
  (unless (plist-get data :version)
    (error "Missing version information in KB file"))
  
  (unless (string= (plist-get data :version) kb-persistence-file-version)
    (warn "KB file version mismatch: expected %s, got %s"
          kb-persistence-file-version (plist-get data :version)))
  
  (unless (plist-get data :timestamp)
    (error "Missing timestamp in KB file"))
  
  t)

(defun kb-validate-consistency ()
  "Validate consistency of loaded KB data."
  (let ((errors nil))
    
    ;; Check microtheory references
    (maphash (lambda (name mt)
               (let ((parents (kb-microtheory-parent-mts mt)))
                 (dolist (parent parents)
                   (when (and parent (not (kb-get-microtheory parent)))
                     (push (format "Microtheory %s references non-existent parent %s" 
                                 name parent) errors)))))
             kb-microtheories)
    
    ;; Check fact microtheory references
    (maphash (lambda (name mt)
               (dolist (fact (kb-microtheory-facts mt))
                 (unless (kb-get-microtheory (kb-fact-microtheory fact))
                   (push (format "Fact references non-existent microtheory %s" 
                               (kb-fact-microtheory fact)) errors))))
             kb-microtheories)
    
    ;; Check TMS consistency
    (maphash (lambda (key fact-record)
               (dolist (just (kb-fact-record-justifications fact-record))
                 (dolist (premise (kb-justification-premises just))
                   (when premise
                     (let ((premise-key (kb-tms-fact-key 
                                       (nth 0 premise) (nth 1 premise) (nth 2 premise))))
                       (unless (gethash premise-key kb-tms-facts)
                         (push (format "TMS justification references non-existent fact %s" 
                                     premise-key) errors)))))))
             kb-tms-facts)
    
    (if errors
        (progn
          (message "KB consistency validation found %d errors:" (length errors))
          (dolist (error errors)
            (message "  %s" error))
          nil)
      (message "KB consistency validation passed")
      t)))

;;; Backup management

(defun kb-create-backup (filename)
  "Create a backup of the existing file."
  (when (file-exists-p filename)
    (let ((backup-name (format "%s.backup.%d"
                              filename
                              (floor (float-time (current-time))))))
      (copy-file filename backup-name)
      (kb-cleanup-old-backups filename))))

(defun kb-cleanup-old-backups (filename)
  "Remove old backup files, keeping only the most recent ones."
  (let* ((directory (file-name-directory filename))
         (base-name (file-name-nondirectory filename))
         (pattern (concat "^" (regexp-quote base-name) "\\.backup\\.[0-9]+$"))
         (backups nil))
    
    (dolist (file (directory-files directory))
      (when (string-match pattern file)
        (push (cons file (file-attribute-modification-time
                         (file-attributes (expand-file-name file directory))))
              backups)))
    
    ;; Sort by timestamp, newest first
    (setq backups (sort backups (lambda (a b) (time-less-p (cdr b) (cdr a)))))
    
    ;; Remove old backups
    (when (> (length backups) kb-persistence-backup-count)
      (dolist (backup (nthcdr kb-persistence-backup-count backups))
        (delete-file (expand-file-name (car backup) directory))))))

;;; High-level API

;;;###autoload
(defun kb-save-to-file (filename)
  "Interactive command to save KB to file."
  (interactive "FSave KB to: ")
  (kb-persist-save filename))

;;;###autoload
(defun kb-load-from-file (filename)
  "Interactive command to load KB from file."
  (interactive "fLoad KB from: ")
  (kb-persist-load filename))

;;;###autoload
(defun kb-persist-backup ()
  "Create a backup of the current KB state."
  (interactive)
  (let ((backup-file (format "kb-backup-%s.el"
                            (format-time-string "%Y%m%d-%H%M%S"))))
    (kb-persist-save backup-file)
    (message "KB backed up to %s" backup-file)))

;;;###autoload
(defun kb-auto-save-enable (interval filename)
  "Enable automatic saving every INTERVAL seconds to FILENAME."
  (interactive "nAuto-save interval (seconds): \nFAuto-save file: ")
  (when (boundp 'kb-auto-save-timer)
    (cancel-timer kb-auto-save-timer))
  
  (setq kb-auto-save-timer
        (run-at-time interval interval
                     (lambda () (kb-persist-save filename t)))))

;;;###autoload
(defun kb-auto-save-disable ()
  "Disable automatic saving."
  (interactive)
  (when (boundp 'kb-auto-save-timer)
    (cancel-timer kb-auto-save-timer)
    (setq kb-auto-save-timer nil)))

;;; Integration hooks

(defun kb-persistence-fact-added-hook (fact)
  "Hook called when a fact is added."
  (kb-mark-changed 'fact (kb-fact-signature fact)))

(defun kb-persistence-rule-added-hook (rule)
  "Hook called when a rule is added."
  (kb-mark-changed 'rule (kb-rule-name rule)))

(defun kb-persistence-microtheory-changed-hook (mt-name)
  "Hook called when a microtheory is changed."
  (kb-mark-changed 'microtheory mt-name))

;;; Utility functions

(defun kb-fact-signature (fact)
  "Generate a unique signature for a fact."
  (list (kb-fact-subject fact)
        (kb-fact-predicate fact)
        (kb-fact-object fact)
        (kb-fact-microtheory fact)))

(defun kb-persistence-status ()
  "Display persistence system status."
  (interactive)
  (message "KB Persistence v%s | Last full save: %s | Changed items: %d"
           kb-persistence-file-version
           (if kb-persistence-last-full-save
               (format-time-string "%Y-%m-%d %H:%M:%S" kb-persistence-last-full-save)
             "Never")
           (hash-table-count kb-persistence-changed-items)))

(provide 'kb-persistence)
;;; kb-persistence.el ends here
