;;; kb-debugger.el --- Knowledge Base Debugging and Introspection Tools

;; Author: AI Assistant
;; Keywords: ai, debugging, introspection, visualization
;; Version: 2.1

;;; Commentary:

;; This package provides debugging and introspection tools for the knowledge base
;; system, including inference tracing, visualization, and validation tools.

;;; Code:

(require 'cl-lib)
(require 'kb-microtheories)
(require 'kb-inference-engine)

;;; Debug Structures

(cl-defstruct (kb-debug-trace (:constructor kb-debug-trace-create)
                              (:copier nil))
  "A trace of an inference step."
  step-number
  operation              ; :query, :rule-application, :default-application
  input                  ; what went in
  output                 ; what came out
  worker                 ; which inference worker was used
  microtheory           ; context
  justification         ; why this step was taken
  timestamp
  duration)

(cl-defstruct (kb-debug-session (:constructor kb-debug-session-create)
                                (:copier nil))
  "A debugging session."
  id
  start-time
  query
  microtheory
  traces                 ; list of debug traces
  final-result
  total-duration
  status)                ; :running, :completed, :failed

;;; Variables

(defvar kb-debug-enabled nil
  "Whether debugging is currently enabled.")

(defvar kb-debug-current-session nil
  "Current debugging session.")

(defvar kb-debug-sessions (make-hash-table :test 'equal)
  "Hash table of all debug sessions.")

(defvar kb-debug-session-counter 0
  "Counter for generating debug session IDs.")

(defvar kb-debug-max-sessions 50
  "Maximum number of debug sessions to keep.")

;;; Core Debugging Functions

(defun kb-debug-enable ()
  "Enable knowledge base debugging."
  (interactive)
  (setq kb-debug-enabled t)
  (message "KB debugging enabled"))

(defun kb-debug-disable ()
  "Disable knowledge base debugging."
  (interactive)
  (setq kb-debug-enabled nil)
  (setq kb-debug-current-session nil)
  (message "KB debugging disabled"))

(defun kb-debug-start-session (query &optional mt)
  "Start a new debugging session."
  (when kb-debug-enabled
    (let* ((session-id (format "debug-%d" (cl-incf kb-debug-session-counter)))
           (session (kb-debug-session-create
                    :id session-id
                    :start-time (current-time)
                    :query query
                    :microtheory (or mt kb-current-mt)
                    :traces nil
                    :status :running)))
      (setq kb-debug-current-session session)
      (puthash session-id session kb-debug-sessions)
      
      ;; Clean up old sessions if we have too many
      (when (> (hash-table-count kb-debug-sessions) kb-debug-max-sessions)
        (kb-debug-cleanup-old-sessions))
      
      session-id)))

(defun kb-debug-end-session (result)
  "End the current debugging session."
  (when (and kb-debug-enabled kb-debug-current-session)
    (setf (kb-debug-session-final-result kb-debug-current-session) result)
    (setf (kb-debug-session-status kb-debug-current-session) :completed)
    (setf (kb-debug-session-total-duration kb-debug-current-session)
          (float-time (time-subtract (current-time) 
                                    (kb-debug-session-start-time kb-debug-current-session))))
    (let ((session-id (kb-debug-session-id kb-debug-current-session)))
      (setq kb-debug-current-session nil)
      session-id)))

(defun kb-debug-trace (operation input output &optional worker justification)
  "Add a trace step to the current debugging session."
  (when (and kb-debug-enabled kb-debug-current-session)
    (let* ((step-num (1+ (length (kb-debug-session-traces kb-debug-current-session))))
           (trace (kb-debug-trace-create
                  :step-number step-num
                  :operation operation
                  :input input
                  :output output
                  :worker worker
                  :microtheory (kb-debug-session-microtheory kb-debug-current-session)
                  :justification justification
                  :timestamp (current-time))))
      (push trace (kb-debug-session-traces kb-debug-current-session)))))

(defun kb-debug-cleanup-old-sessions ()
  "Remove old debug sessions to free memory."
  (let ((sessions-to-remove nil))
    (maphash (lambda (id session)
               (push (cons (kb-debug-session-start-time session) id) sessions-to-remove))
             kb-debug-sessions)
    
    ;; Sort by time and remove oldest
    (setq sessions-to-remove (sort sessions-to-remove 
                                  (lambda (a b) (time-less-p (car a) (car b)))))
    
    (let ((to-remove (- (hash-table-count kb-debug-sessions) kb-debug-max-sessions)))
      (dotimes (i to-remove)
        (remhash (cdar sessions-to-remove) kb-debug-sessions)
        (setq sessions-to-remove (cdr sessions-to-remove))))))

;;; Enhanced Query Functions with Debugging

(defun kb-debug-query (subject predicate &optional mt)
  "Query with debugging enabled."
  (if kb-debug-enabled
      (let ((session-id (kb-debug-start-session `(,subject ,predicate) mt)))
        (kb-debug-trace :query `(,subject ,predicate) nil)
        (let ((result (kb-query-with-inheritance subject predicate mt)))
          (kb-debug-trace :query-result `(,subject ,predicate) result)
          (kb-debug-end-session result)
          result))
    (kb-query-with-inheritance subject predicate mt)))

(defun kb-debug-inference (query &optional mt timeout)
  "Run inference with debugging."
  (if kb-debug-enabled
      (let ((session-id (kb-debug-start-session query mt)))
        (kb-debug-trace :inference-start query nil)
        (let ((result (kb-inference-strategist query mt timeout)))
          (kb-debug-trace :inference-complete query result)
          (kb-debug-end-session result)
          result))
    (kb-inference-strategist query mt timeout)))

;;; Debugging Display Functions

(defun kb-debug-show-session (session-id)
  "Show details of a debugging session."
  (interactive (list (completing-read "Session ID: " 
                                     (kb-debug-list-session-ids))))
  (let ((session (gethash session-id kb-debug-sessions)))
    (if session
        (kb-debug-display-session session)
      (message "Debug session %s not found" session-id))))

(defun kb-debug-list-session-ids ()
  "Get list of all debug session IDs."
  (let ((ids nil))
    (maphash (lambda (id session) (push id ids)) kb-debug-sessions)
    (sort ids #'string<)))

(defun kb-debug-display-session (session)
  "Display a debug session in a buffer."
  (let ((buffer-name (format "*KB Debug: %s*" (kb-debug-session-id session))))
    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (insert (format "=== Knowledge Base Debug Session ===\n"))
      (insert (format "Session ID: %s\n" (kb-debug-session-id session)))
      (insert (format "Query: %s\n" (kb-debug-session-query session)))
      (insert (format "Microtheory: %s\n" (kb-debug-session-microtheory session)))
      (insert (format "Status: %s\n" (kb-debug-session-status session)))
      (insert (format "Duration: %.4f seconds\n" (kb-debug-session-total-duration session)))
      (insert (format "Steps: %d\n\n" (length (kb-debug-session-traces session))))
      
      (insert "=== Trace Steps ===\n")
      (dolist (trace (reverse (kb-debug-session-traces session)))
        (insert (format "Step %d: %s\n" 
                       (kb-debug-trace-step-number trace)
                       (kb-debug-trace-operation trace)))
        (insert (format "  Input: %s\n" (kb-debug-trace-input trace)))
        (insert (format "  Output: %s\n" (kb-debug-trace-output trace)))
        (when (kb-debug-trace-worker trace)
          (insert (format "  Worker: %s\n" (kb-debug-trace-worker trace))))
        (when (kb-debug-trace-justification trace)
          (insert (format "  Justification: %s\n" (kb-debug-trace-justification trace))))
        (insert "\n"))
      
      (insert (format "\n=== Final Result ===\n"))
      (insert (format "%s\n" (kb-debug-session-final-result session)))
      
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

;;; Knowledge Base Validation

(defun kb-validate-microtheory (mt-name)
  "Validate consistency of a microtheory."
  (interactive (list (completing-read "Microtheory: " 
                                     (kb-list-microtheories))))
  (let ((mt (kb-get-microtheory (intern mt-name)))
        (inconsistencies nil)
        (warnings nil))
    
    (when mt
      ;; Check for contradictory facts
      (maphash 
       (lambda (subject facts)
         (let ((predicates (make-hash-table :test 'equal)))
           (dolist (fact facts)
             (let ((pred (kb-fact-predicate fact))
                   (obj (kb-fact-object fact)))
               (when (gethash pred predicates)
                 (unless (equal obj (gethash pred predicates))
                   (push (format "Contradiction: %s %s has values %s and %s"
                                subject pred obj (gethash pred predicates))
                         inconsistencies)))
               (puthash pred obj predicates)))))
       (kb-microtheory-facts mt))
      
      ;; Check for circular inheritance
      (let ((visited (make-hash-table :test 'equal)))
        (kb-check-circular-inheritance mt-name visited))
      
      ;; Display results
      (let ((buffer-name (format "*KB Validation: %s*" mt-name)))
        (with-current-buffer (get-buffer-create buffer-name)
          (erase-buffer)
          (insert (format "=== Validation Report for %s ===\n\n" mt-name))
          
          (if inconsistencies
              (progn
                (insert "INCONSISTENCIES FOUND:\n")
                (dolist (inc inconsistencies)
                  (insert (format "- %s\n" inc))))
            (insert "✓ No inconsistencies found\n"))
          
          (when warnings
            (insert "\nWARNINGS:\n")
            (dolist (warn warnings)
              (insert (format "- %s\n" warn))))
          
          (insert (format "\nFacts count: %d\n" 
                         (kb-count-facts-in-mt mt)))
          (insert (format "Rules count: %d\n" 
                         (length (kb-microtheory-rules mt))))
          
          (goto-char (point-min))
          (pop-to-buffer (current-buffer)))))))

(defun kb-count-facts-in-mt (mt)
  "Count total facts in a microtheory."
  (let ((count 0))
    (maphash (lambda (subject facts)
               (setq count (+ count (length facts))))
             (kb-microtheory-facts mt))
    count))

(defun kb-check-circular-inheritance (mt-name visited)
  "Check for circular inheritance in microtheory hierarchy."
  (when (gethash mt-name visited)
    (error "Circular inheritance detected involving %s" mt-name))
  
  (puthash mt-name t visited)
  
  (let ((mt (kb-get-microtheory mt-name)))
    (when mt
      (dolist (parent (kb-microtheory-parent-mts mt))
        (kb-check-circular-inheritance parent visited))))
  
  (remhash mt-name visited))

;;; Interactive Debugging Commands

;;;###autoload
(defun kb-debug-last-query ()
  "Show debug information for the last query."
  (interactive)
  (if kb-debug-current-session
      (kb-debug-display-session kb-debug-current-session)
    (let ((sessions (kb-debug-list-session-ids)))
      (if sessions
          (kb-debug-show-session (car (sort sessions #'string>)))
        (message "No debug sessions found")))))

;;;###autoload
(defun kb-debug-clear-sessions ()
  "Clear all debug sessions."
  (interactive)
  (clrhash kb-debug-sessions)
  (setq kb-debug-current-session nil)
  (setq kb-debug-session-counter 0)
  (message "All debug sessions cleared"))

;;;###autoload
(defun kb-debug-stats ()
  "Show debugging statistics."
  (interactive)
  (message "Debug sessions: %d, Enabled: %s, Current session: %s"
           (hash-table-count kb-debug-sessions)
           kb-debug-enabled
           (if kb-debug-current-session 
               (kb-debug-session-id kb-debug-current-session)
             "none")))

;;; Visualization Functions

(defun kb-visualize-microtheory (mt-name &optional max-facts)
  "Create a simple text visualization of a microtheory."
  (interactive (list (completing-read "Microtheory: " 
                                     (kb-list-microtheories))
                     current-prefix-arg))
  (let ((mt (kb-get-microtheory (intern mt-name)))
        (max-facts (or max-facts 50)))
    
    (when mt
      (let ((buffer-name (format "*KB Visualization: %s*" mt-name)))
        (with-current-buffer (get-buffer-create buffer-name)
          (erase-buffer)
          (insert (format "=== Microtheory: %s ===\n\n" mt-name))
          
          ;; Show inheritance
          (when (kb-microtheory-parent-mts mt)
            (insert "Inherits from: ")
            (insert (mapconcat #'symbol-name (kb-microtheory-parent-mts mt) ", "))
            (insert "\n\n"))
          
          ;; Show facts
          (insert "Facts:\n")
          (let ((fact-count 0))
            (catch 'max-reached
              (maphash 
               (lambda (subject facts)
                 (dolist (fact facts)
                   (when (>= fact-count max-facts)
                     (insert (format "... (%d more facts)\n" 
                                    (- (kb-count-facts-in-mt mt) max-facts)))
                     (throw 'max-reached nil))
                   (insert (format "  %s %s %s (%.2f)\n"
                                  (kb-fact-subject fact)
                                  (kb-fact-predicate fact)
                                  (kb-fact-object fact)
                                  (kb-fact-certainty fact)))
                   (cl-incf fact-count)))
               (kb-microtheory-facts mt))))
          
          ;; Show rules
          (when (kb-microtheory-rules mt)
            (insert "\nRules:\n")
            (dolist (rule (kb-microtheory-rules mt))
              (insert (format "  %s: %s => %s\n"
                             (kb-rule-name rule)
                             (kb-rule-premises rule)
                             (kb-rule-conclusion rule)))))
          
          (goto-char (point-min))
          (pop-to-buffer (current-buffer)))))))

(provide 'kb-debugger)
;;; kb-debugger.el ends here
