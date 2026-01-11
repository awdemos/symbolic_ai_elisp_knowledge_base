;;; kb-debugger.el --- Knowledge Base Debugging and Introspection Tools
;; -*- lexical-binding: t; -*-

;; Author: AI Assistant
;; Keywords: ai, debugging, introspection, visualization
;; Version: 2.1

;;; Commentary:

;; This package provides debugging and introspection tools for the knowledge base
;; system, including inference tracing, visualization, and validation tools.

;; Features:
;; - Inference step-by-step tracing
;; - Knowledge base state visualization
;; - Performance profiling
;; - Interactive query debugging
;; - Rule firing analysis
;; - Fact dependency tracking

;;; Code:

(require 'cl-lib)
(require 'kb-microtheories)
(require 'kb-tms)

;;; Variables

(defvar kb-debug-enabled nil
  "Whether debugging is enabled.")

(defvar kb-debug-trace-level 0
  "Debug trace level (0=off, 1=basic, 2=detailed, 3=verbose).")

(defvar kb-debug-log-max-size 1000
  "Maximum number of debug entries to keep.")

(defvar kb-debug-log nil
  "List of debug log entries.")

(defvar kb-debug-profile-data nil
  "Hash table for profiling data.")

;;; Structures

(cl-defstruct (kb-debug-entry (:constructor kb-debug-entry-create)
                            (:copier nil))
  timestamp
  operation
  details
  microtheory
  duration)

(cl-defstruct (kb-profile-entry (:constructor kb-profile-entry-create)
                           (:copier nil))
  operation
  count
  total-time
  avg-time
  min-time
  max-time)

;;; Debug Logging API

;;;###autoload
(defun kb-debug-log-entry (operation &rest details)
  "Log a debug entry.
OPERATION is the name of the operation being logged.
DETAILS are additional information to log."
  (when kb-debug-enabled
    (let ((entry (kb-debug-entry-create
                 :timestamp (current-time)
                 :operation operation
                 :details details
                 :microtheory kb-current-mt
                 :duration nil)))
      (push entry kb-debug-log)
      ;; Prune log if too large
      (when (> (length kb-debug-log) kb-debug-log-max-size)
        (setq kb-debug-log (cl-subseq kb-debug-log 0 kb-debug-log-max-size))))))

;;;###autoload
(defun kb-debug-show-log (&optional count)
  "Show recent debug log entries.
COUNT is the number of recent entries to show (default 10)."
  (interactive "nShow debug log (recent N): ")
  (let ((n (or count 10)))
    (with-output-to-temp-buffer "*KB Debug Log*"
      (princ (format "KB Debug Log - Last %d entries:\n\n" n))
      (dolist (entry (cl-subseq kb-debug-log 0 (min n (length kb-debug-log))))
        (let ((time-str (format-time-string "%Y-%m-%d %H:%M:%S" (kb-debug-entry-timestamp entry))))
          (princ (format "[%s] %s\n" time-str (kb-debug-entry-operation entry)))
          (princ "  Details:\n")
          (dolist (detail (kb-debug-entry-details entry))
            (princ (format "    %s\n" detail)))
          (princ "  Microtheory: ")
          (princ (format "%s\n" (or (kb-debug-entry-microtheory entry) "N/A")))
          (princ "\n")))
      (help-mode))))

;;;###autoload
(defun kb-debug-clear-log ()
  "Clear the debug log."
  (interactive)
  (setq kb-debug-log nil)
  (message "Debug log cleared"))

;;; Profiling API

;;;###autoload
(defun kb-debug-profile-start (operation)
  "Start profiling an operation.
OPERATION is the name of the operation to profile."
  (puthash operation (list 0 0.0 0.0 most-positive-fixnum 0.0) kb-debug-profile-data))

;;;###autoload
(defun kb-debug-profile-end (operation)
  "End profiling an operation and record statistics.
OPERATION is the name of the operation being profiled."
  (let ((start-time (gethash operation kb-debug-profile-data))
        (end-time (float-time))
        (duration (- end-time start-time)))
    (when start-time
      (let ((entry (gethash operation kb-debug-profile-data)))
        (setf (kb-profile-entry-count entry) (1+ (kb-profile-entry-count entry)))
        (setf (kb-profile-entry-total-time entry) (+ (kb-profile-entry-total-time entry) duration))
        (setf (kb-profile-entry-avg-time entry) (/ (kb-profile-entry-total-time entry) (kb-profile-entry-count entry)))
        (setf (kb-profile-entry-min-time entry) (min (kb-profile-entry-min-time entry) duration))
        (setf (kb-profile-entry-max-time entry) (max (kb-profile-entry-max-time entry) duration)))))))

;;;###autoload
(defun kb-debug-show-profile ()
  "Show profiling statistics."
  (interactive)
  (unless kb-debug-profile-data
    (setq kb-debug-profile-data (make-hash-table :test 'equal)))
  (with-output-to-temp-buffer "*KB Profile*"
    (princ "KB Profiling Statistics:\n\n")
    (maphash (lambda (op entry)
                (princ (format "%s:\n" op))
                (princ (format "  Calls: %d\n" (kb-profile-entry-count entry)))
                (princ (format "  Total time: %.4f sec\n" (kb-profile-entry-total-time entry)))
                (princ (format "  Avg time: %.4f sec\n" (kb-profile-entry-avg-time entry)))
                (princ (format "  Min time: %.4f sec\n" (kb-profile-entry-min-time entry)))
                (princ (format "  Max time: %.4f sec\n" (kb-profile-entry-max-time entry)))
                (princ "\n"))
              kb-debug-profile-data))
    (help-mode)))

;;;###autoload
(defun kb-debug-clear-profile ()
  "Clear profiling data."
  (interactive)
  (setq kb-debug-profile-data nil)
  (message "Profile data cleared"))

;;; Knowledge Base Inspection

;;;###autoload
(defun kb-debug-inspect-microtheory (mt-name)
  "Inspect a microtheory in detail.
MT-NAME is the name of the microtheory to inspect."
  (interactive "sInspect microtheory: ")
  (let ((mt (kb-get-microtheory mt-name)))
    (unless mt
      (error "Microtheory not found: %s" mt-name))
    (with-output-to-temp-buffer (format "*KB Microtheory: %s*" mt-name)
      (princ (format "Microtheory: %s\n" mt-name))
      (princ (format "Priority: %s\n" (kb-microtheory-priority mt)))
      (princ (format "Inheritance mode: %s\n" (kb-microtheory-inheritance-mode mt)))
      (princ "\nParents:\n")
      (dolist (parent (kb-microtheory-parent-mts mt))
        (princ (format "  %s\n" parent)))
      (princ "\nAncestors:\n")
      (dolist (ancestor (kb-get-ancestors mt-name))
        (princ (format "  %s\n" ancestor)))
      (princ "\nFacts:\n")
      (maphash (lambda (subject facts)
                  (dolist (fact facts)
                    (princ (format "  (%s %s %s)\n" 
                                   subject 
                                   (kb-fact-predicate fact)
                                   (kb-fact-object fact))))
                (kb-microtheory-facts mt))
      (princ "\nRules:\n")
      (dolist (rule (kb-microtheory-rules mt))
        (princ (format "  %s\n" (kb-rule-name rule)))
        (princ (format "    Premises: %s\n" (kb-rule-premises rule)))
        (princ (format "    Conclusion: %s\n" (kb-rule-conclusion rule)))
      (help-mode))))

;;;###autoload
(defun kb-debug-show-all-microtheories ()
  "Show overview of all microtheories."
  (interactive)
  (with-output-to-temp-buffer "*KB Microtheories*"
    (maphash (lambda (name mt)
                (princ (format "%s (priority: %s, %d facts, %d rules)\n"
                               name
                               (kb-microtheory-priority mt)
                               (hash-table-count (kb-microtheory-facts mt))
                               (length (kb-microtheory-rules mt))))
              kb-microtheories)
    (help-mode)))

;;; TMS Integration

;;;###autoload
(defun kb-debug-show-justifications (subject predicate)
  "Show TMS justifications for a fact.
SUBJECT and PREDICATE identify the fact."
  (interactive "sSubject: \nsPredicate: ")
  (when (boundp 'kb-tms-facts)
    (let ((fact-key (kb-tms-fact-key subject predicate nil)))
          (fact-record (gethash fact-key kb-tms-facts)))
      (when fact-record
        (let ((justifications (kb-fact-record-justifications fact-record)))
          (if justifications
              (with-output-to-temp-buffer "*KB Justifications*"
                (princ (format "Justifications for (%s %s):\n\n" subject predicate))
                (dolist (just (cl-remove-duplicates justifications))
                  (princ (format "Type: %s\n" (car just)))
                  (let ((just-type (car just)))
                    (cond
                     ((eq just-type :direct)
                      (princ "  Source: Direct assertion\n"))
                     ((eq just-type :derived)
                      (let ((premises (kb-justification-premises just)))
                        (princ "  Derived from premises:\n")
                        (dolist (premise premises)
                          (princ (format "    %s\n" premise)))))
                     ((eq just-type :default)
                      (princ (format "  Default rule: %s\n" (cadr just))))
                     ((eq just-type :exception)
                      (princ (format "  Exception to rule: %s\n" (cadr just))))))
                  (princ "\n")))
                (help-mode))
            (message "No justifications found for (%s %s)" subject predicate)))))))

;;; Debug Control

;;;###autoload
(defun kb-debug-enable ()
  "Enable debugging."
  (interactive)
  (setq kb-debug-enabled t)
  (message "KB debugging enabled"))

;;;###autoload
(defun kb-debug-disable ()
  "Disable debugging."
  (interactive)
  (setq kb-debug-enabled nil)
  (message "KB debugging disabled"))

;;;###autoload
(defun kb-debug-set-trace-level (level)
  "Set the debug trace level.
LEVEL should be 0 (off), 1 (basic), 2 (detailed), or 3 (verbose)."
  (interactive "nTrace level (0-3): ")
  (if (and (integerp level) (<= 0 level 3))
      (progn
        (setq kb-debug-trace-level level)
        (message "Debug trace level set to %d" level))
    (error "Invalid trace level: %s (must be 0-3)" level)))

;;;###autoload
(defun kb-debug-show-config ()
  "Show current debug configuration."
  (interactive)
  (message "KB Debug Configuration:")
  (message "  Enabled: %s" kb-debug-enabled)
  (message "  Trace level: %d" kb-debug-trace-level)
  (message "  Max log size: %d" kb-debug-log-max-size)
  (message "  Log entries: %d" (length kb-debug-log))
  (message "  Profiled operations: %d" (hash-table-count kb-debug-profile-data)))

(provide 'kb-debugger)
;;; kb-debugger.el ends here
