;;; kb-database.el --- Database Integration for Knowledge Base System

;; Author: AI Assistant
;; Keywords: ai, database, sqlite, persistence, storage
;; Version: 2.1

;;; Commentary:

;; This package provides database integration for the knowledge base system,
;; starting with SQLite support for persistent storage and data import/export.

;;; Code:

(require 'cl-lib)
(require 'sql)
(require 'kb-microtheories)

;;; Database Structures

(cl-defstruct (kb-db-connection (:constructor kb-db-connection-create)
                                (:copier nil))
  "A database connection."
  type                   ; :sqlite, :postgresql, :mysql
  file-path              ; for SQLite
  host                   ; for network databases  
  port
  database
  user
  password
  connection             ; actual SQL connection
  schema-version         ; KB schema version
  connected-p)

(cl-defstruct (kb-db-mapping (:constructor kb-db-mapping-create)
                             (:copier nil))
  "Mapping between KB concepts and database schema."
  table-name
  subject-column
  predicate-column
  object-column
  microtheory-column
  certainty-column
  timestamp-column)

;;; Variables

(defvar kb-db-connection nil
  "Current database connection.")

(defvar kb-db-schema-version "1.0"
  "Current KB database schema version.")

(defvar kb-db-default-mapping
  (kb-db-mapping-create
   :table-name "kb_facts"
   :subject-column "subject"
   :predicate-column "predicate"
   :object-column "object"
   :microtheory-column "microtheory"
   :certainty-column "certainty"
   :timestamp-column "created_at")
  "Default database schema mapping.")

;;; SQLite Integration

(defun kb-db-connect-sqlite (db-path)
  "Connect to SQLite database."
  (interactive "fSQLite database file: ")
  (let ((connection (kb-db-connection-create
                    :type :sqlite
                    :file-path (expand-file-name db-path)
                    :connected-p nil
                    :schema-version nil)))
    
    ;; Test connection by creating/opening database
    (condition-case err
        (progn
          (setq kb-db-connection connection)
          (kb-db-ensure-schema)
          (setf (kb-db-connection-connected-p connection) t)
          (message "Connected to SQLite database: %s" db-path)
          connection)
      (error
       (message "Failed to connect to database: %s" (error-message-string err))
       nil))))

(defun kb-db-disconnect ()
  "Disconnect from current database."
  (interactive)
  (when kb-db-connection
    (setf (kb-db-connection-connected-p kb-db-connection) nil)
    (setq kb-db-connection nil)
    (message "Disconnected from database")))

(defun kb-db-ensure-schema ()
  "Ensure the database schema exists."
  (when (and kb-db-connection 
             (eq (kb-db-connection-type kb-db-connection) :sqlite))
    
    ;; Create main facts table
    (kb-db-execute-sql
     "CREATE TABLE IF NOT EXISTS kb_facts (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        subject TEXT NOT NULL,
        predicate TEXT NOT NULL,
        object TEXT NOT NULL,
        object_type TEXT DEFAULT 'literal',
        microtheory TEXT NOT NULL,
        certainty REAL DEFAULT 1.0,
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
      )")
    
    ;; Create microtheories table
    (kb-db-execute-sql
     "CREATE TABLE IF NOT EXISTS kb_microtheories (
        name TEXT PRIMARY KEY,
        parent_mt TEXT,
        description TEXT,
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
      )")
    
    ;; Create rules table
    (kb-db-execute-sql
     "CREATE TABLE IF NOT EXISTS kb_rules (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        name TEXT NOT NULL,
        premises TEXT NOT NULL,  -- JSON array
        conclusion TEXT NOT NULL, -- JSON array
        microtheory TEXT NOT NULL,
        priority REAL DEFAULT 1.0,
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
      )")
    
    ;; Create indexes for performance
    (kb-db-execute-sql
     "CREATE INDEX IF NOT EXISTS idx_facts_subject ON kb_facts(subject)")
    (kb-db-execute-sql
     "CREATE INDEX IF NOT EXISTS idx_facts_predicate ON kb_facts(predicate)")
    (kb-db-execute-sql
     "CREATE INDEX IF NOT EXISTS idx_facts_microtheory ON kb_facts(microtheory)")
    
    ;; Create metadata table
    (kb-db-execute-sql
     "CREATE TABLE IF NOT EXISTS kb_metadata (
        key TEXT PRIMARY KEY,
        value TEXT,
        updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
      )")
    
    ;; Store schema version
    (kb-db-execute-sql
     "INSERT OR REPLACE INTO kb_metadata (key, value) VALUES (?, ?)"
     "schema_version" kb-db-schema-version)
    
    (message "Database schema initialized")))

(defun kb-db-execute-sql (sql &rest params)
  "Execute SQL with parameters."
  (when (and kb-db-connection 
             (kb-db-connection-connected-p kb-db-connection))
    (let ((db-path (kb-db-connection-file-path kb-db-connection)))
      (with-temp-buffer
        (let ((process-environment 
               (cons (format "SQLITE_DATABASE=%s" db-path)
                     process-environment)))
          (apply #'call-process "sqlite3" nil t nil
                 db-path
                 (if params
                     (list "-cmd" (format sql (mapcar #'kb-db-escape-param params)))
                   (list sql))))
        (buffer-string)))))

(defun kb-db-escape-param (param)
  "Escape a parameter for SQL."
  (cond
   ((stringp param) (format "'%s'" (replace-regexp-in-string "'" "''" param)))
   ((numberp param) (number-to-string param))
   ((symbolp param) (format "'%s'" (symbol-name param)))
   (t (format "'%S'" param))))

;;; Knowledge Base Persistence

(defun kb-db-save-microtheory (mt-name)
  "Save a microtheory to the database."
  (interactive (list (completing-read "Microtheory: " (kb-list-microtheories))))
  (unless kb-db-connection
    (error "No database connection"))
  
  (let ((mt (kb-get-microtheory (intern mt-name))))
    (unless mt
      (error "Microtheory %s not found" mt-name))
    
    ;; Save microtheory metadata
    (kb-db-execute-sql
     "INSERT OR REPLACE INTO kb_microtheories (name, parent_mt, description) 
      VALUES (?, ?, ?)"
     mt-name
     (if (kb-microtheory-parent-mts mt)
         (symbol-name (car (kb-microtheory-parent-mts mt)))
       "")
     (format "Microtheory %s" mt-name))
    
    ;; Save facts
    (let ((fact-count 0))
      (maphash (lambda (subject facts)
                 (dolist (fact facts)
                   (kb-db-save-fact fact)
                   (cl-incf fact-count)))
               (kb-microtheory-facts mt))
      
      ;; Save rules
      (dolist (rule (kb-microtheory-rules mt))
        (kb-db-save-rule rule))
      
      (message "Saved %d facts from microtheory %s to database" 
               fact-count mt-name))))

(defun kb-db-save-fact (fact)
  "Save a single fact to the database."
  (let ((object-type (cond
                      ((symbolp (kb-fact-object fact)) "symbol")
                      ((numberp (kb-fact-object fact)) "number")
                      ((stringp (kb-fact-object fact)) "string")
                      (t "other"))))
    
    (kb-db-execute-sql
     "INSERT OR REPLACE INTO kb_facts 
      (subject, predicate, object, object_type, microtheory, certainty)
      VALUES (?, ?, ?, ?, ?, ?)"
     (symbol-name (kb-fact-subject fact))
     (symbol-name (kb-fact-predicate fact))
     (format "%S" (kb-fact-object fact))
     object-type
     (symbol-name (kb-fact-microtheory fact))
     (kb-fact-certainty fact))))

(defun kb-db-save-rule (rule)
  "Save a single rule to the database."
  (kb-db-execute-sql
   "INSERT OR REPLACE INTO kb_rules 
    (name, premises, conclusion, microtheory, priority)
    VALUES (?, ?, ?, ?, ?)"
   (symbol-name (kb-rule-name rule))
   (json-encode (kb-rule-premises rule))
   (json-encode (kb-rule-conclusion rule))
   (symbol-name (kb-rule-microtheory rule))
   (or (kb-rule-priority rule) 1.0)))

(defun kb-db-load-microtheory (mt-name)
  "Load a microtheory from the database."
  (interactive (list (completing-read "Microtheory: " (kb-db-list-microtheories))))
  (unless kb-db-connection
    (error "No database connection"))
  
  ;; Create or get microtheory
  (let ((mt (or (kb-get-microtheory (intern mt-name))
                (kb-create-microtheory (intern mt-name)))))
    
    ;; Clear existing facts
    (clrhash (kb-microtheory-facts mt))
    (setf (kb-microtheory-rules mt) nil)
    
    ;; Load facts
    (let ((facts-result (kb-db-query-facts mt-name))
          (loaded-count 0))
      
      (dolist (row facts-result)
        (kb-db-restore-fact row mt-name)
        (cl-incf loaded-count))
      
      ;; Load rules
      (let ((rules-result (kb-db-query-rules mt-name)))
        (dolist (row rules-result)
          (kb-db-restore-rule row mt-name)))
      
      (message "Loaded %d facts into microtheory %s from database" 
               loaded-count mt-name))))

(defun kb-db-query-facts (mt-name)
  "Query facts for a microtheory from database."
  (let ((result (kb-db-execute-sql
                 "SELECT subject, predicate, object, object_type, certainty 
                  FROM kb_facts WHERE microtheory = ?" mt-name)))
    (kb-db-parse-query-result result)))

(defun kb-db-query-rules (mt-name)
  "Query rules for a microtheory from database."
  (let ((result (kb-db-execute-sql
                 "SELECT name, premises, conclusion, priority 
                  FROM kb_rules WHERE microtheory = ?" mt-name)))
    (kb-db-parse-query-result result)))

(defun kb-db-parse-query-result (result-string)
  "Parse SQLite query result into list of rows."
  (let ((lines (split-string (string-trim result-string) "\n"))
        (rows nil))
    (dolist (line lines)
      (when (not (string-empty-p line))
        (let ((fields (split-string line "|")))
          (push (mapcar #'string-trim fields) rows))))
    (reverse rows)))

(defun kb-db-restore-fact (row mt-name)
  "Restore a fact from database row."
  (when (>= (length row) 5)
    (let ((subject (intern (nth 0 row)))
          (predicate (intern (nth 1 row)))
          (object-str (nth 2 row))
          (object-type (nth 3 row))
          (certainty (string-to-number (nth 4 row))))
      
      ;; Convert object based on type
      (let ((object (case (intern object-type)
                      (symbol (intern object-str))
                      (number (string-to-number object-str))
                      (string object-str)
                      (t (read object-str)))))
        
        (kb-with-microtheory (intern mt-name)
          (kb-add-fact subject predicate object certainty))))))

(defun kb-db-restore-rule (row mt-name)
  "Restore a rule from database row."
  (when (>= (length row) 4)
    (let ((name (intern (nth 0 row)))
          (premises (json-read-from-string (nth 1 row)))
          (conclusion (json-read-from-string (nth 2 row)))
          (priority (string-to-number (nth 3 row))))
      
      (kb-with-microtheory (intern mt-name)
        (kb-add-rule name premises conclusion priority)))))

;;; Database Queries

(defun kb-db-query-subject (subject &optional mt)
  "Query all facts about a subject from database."
  (let ((result (if mt
                    (kb-db-execute-sql
                     "SELECT predicate, object, object_type, certainty 
                      FROM kb_facts WHERE subject = ? AND microtheory = ?"
                     (symbol-name subject) (symbol-name mt))
                  (kb-db-execute-sql
                   "SELECT predicate, object, object_type, certainty, microtheory
                    FROM kb_facts WHERE subject = ?"
                   (symbol-name subject)))))
    (kb-db-parse-query-result result)))

(defun kb-db-search-facts (pattern)
  "Search for facts matching a pattern."
  (interactive "sSearch pattern: ")
  (let ((result (kb-db-execute-sql
                 "SELECT subject, predicate, object, microtheory, certainty
                  FROM kb_facts 
                  WHERE subject LIKE ? OR predicate LIKE ? OR object LIKE ?"
                 (format "%%%s%%" pattern)
                 (format "%%%s%%" pattern)
                 (format "%%%s%%" pattern))))
    
    (let ((rows (kb-db-parse-query-result result)))
      (if rows
          (progn
            (message "Found %d matching facts:" (length rows))
            (dolist (row rows)
              (message "  %s %s %s (in %s, certainty: %s)"
                      (nth 0 row) (nth 1 row) (nth 2 row) 
                      (nth 3 row) (nth 4 row))))
        (message "No facts found matching pattern: %s" pattern)))))

(defun kb-db-list-microtheories ()
  "Get list of microtheories in database."
  (let ((result (kb-db-execute-sql "SELECT name FROM kb_microtheories")))
    (mapcar #'car (kb-db-parse-query-result result))))

;;; Data Import/Export

(defun kb-db-import-csv (file-path &optional mt-name)
  "Import facts from CSV file."
  (interactive "fCSV file: ")
  (let ((mt-name (or mt-name 'ImportedDataMt))
        (imported-count 0))
    
    (unless (kb-get-microtheory mt-name)
      (kb-create-microtheory mt-name 'CommonSenseMt))
    
    (with-temp-buffer
      (insert-file-contents file-path)
      (goto-char (point-min))
      
      ;; Skip header line if present
      (when (looking-at "subject,predicate,object")
        (forward-line 1))
      
      (while (not (eobp))
        (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
          (when (not (string-empty-p line))
            (let ((fields (split-string line ",")))
              (when (>= (length fields) 3)
                (let ((subject (intern (string-trim (nth 0 fields) "\"")))
                      (predicate (intern (string-trim (nth 1 fields) "\"")))
                      (object (let ((obj-str (string-trim (nth 2 fields) "\"")))
                               (if (string-match-p "^[0-9.]+$" obj-str)
                                   (string-to-number obj-str)
                                 (intern obj-str)))))
                  
                  (kb-with-microtheory mt-name
                    (kb-add-fact subject predicate object))
                  
                  ;; Save to database if connected
                  (when kb-db-connection
                    (kb-db-save-fact (kb-fact-create
                                     :subject subject
                                     :predicate predicate
                                     :object object
                                     :certainty 1.0
                                     :microtheory mt-name)))
                  
                  (cl-incf imported-count)))))
        (forward-line 1)))
    
    (message "Imported %d facts from CSV file into %s" imported-count mt-name)))

(defun kb-db-export-csv (mt-name file-path)
  "Export microtheory facts to CSV file."
  (interactive (list (completing-read "Microtheory: " (kb-list-microtheories))
                     (read-file-name "Output CSV file: ")))
  
  (let ((mt (kb-get-microtheory (intern mt-name)))
        (exported-count 0))
    
    (unless mt
      (error "Microtheory %s not found" mt-name))
    
    (with-temp-file file-path
      (insert "subject,predicate,object,certainty\n")
      
      (maphash (lambda (subject facts)
                 (dolist (fact facts)
                   (insert (format "%s,%s,%s,%.3f\n"
                                  (kb-fact-subject fact)
                                  (kb-fact-predicate fact)
                                  (kb-fact-object fact)
                                  (kb-fact-certainty fact)))
                   (cl-incf exported-count)))
               (kb-microtheory-facts mt)))
    
    (message "Exported %d facts to %s" exported-count file-path)))

;;; Database Statistics and Maintenance

(defun kb-db-stats ()
  "Show database statistics."
  (interactive)
  (unless kb-db-connection
    (error "No database connection"))
  
  (let ((facts-count (kb-db-execute-sql "SELECT COUNT(*) FROM kb_facts"))
        (mts-count (kb-db-execute-sql "SELECT COUNT(*) FROM kb_microtheories"))
        (rules-count (kb-db-execute-sql "SELECT COUNT(*) FROM kb_rules")))
    
    (message "Database Stats - Facts: %s, Microtheories: %s, Rules: %s"
             (string-trim facts-count)
             (string-trim mts-count)
             (string-trim rules-count))))

(defun kb-db-vacuum ()
  "Vacuum database to reclaim space."
  (interactive)
  (when kb-db-connection
    (kb-db-execute-sql "VACUUM")
    (message "Database vacuumed")))

;;; Interactive Commands

;;;###autoload
(defun kb-db-status ()
  "Show database connection status."
  (interactive)
  (if kb-db-connection
      (message "Connected to %s database: %s"
               (kb-db-connection-type kb-db-connection)
               (or (kb-db-connection-file-path kb-db-connection)
                   (format "%s:%s/%s" 
                          (kb-db-connection-host kb-db-connection)
                          (kb-db-connection-port kb-db-connection)
                          (kb-db-connection-database kb-db-connection))))
    (message "No database connection")))

;;;###autoload
(defun kb-db-setup ()
  "Interactive database setup."
  (interactive)
  (let ((db-type (intern (completing-read "Database type: " '("sqlite") nil t "sqlite"))))
    (case db-type
      (:sqlite 
       (let ((db-file (read-file-name "SQLite database file: " nil "kb.sqlite")))
         (kb-db-connect-sqlite db-file))))))

(provide 'kb-database)
;;; kb-database.el ends here
