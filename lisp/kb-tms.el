;;; kb-tms.el --- Truth Maintenance System for KB

;; Author: AI Assistant
;; Keywords: ai, knowledge base, reasoning, tms
;; Version: 1.0

;;; Commentary:

;; This module provides a Truth Maintenance System (TMS) for tracking
;; justifications and dependencies between facts. This enables reliable
;; belief revision and non-monotonic reasoning.

;;; Code:

(require 'cl-lib)

;;; Data structures

(cl-defstruct kb-justification
  "A justification record for a fact."
  fact           ; The fact being justified
  premises       ; List of premise facts
  rule          ; Rule that derived this fact (optional)
  support-type  ; 'direct, 'derived, 'default, 'assumption
  timestamp     ; When this justification was created
  active-p)     ; Whether this justification is currently active

(cl-defstruct kb-fact-record
  "Extended fact record with TMS information."
  subject
  predicate
  object
  microtheory
  justifications    ; List of kb-justification structs
  dependents       ; Facts that depend on this one
  belief-status)   ; 'in, 'out, 'unknown

;;; Global TMS state

(defvar kb-tms-facts (make-hash-table :test 'equal)
  "Hash table of all facts with their TMS records.")

(defvar kb-tms-justifications (make-hash-table :test 'equal)
  "Hash table of all justifications.")

(defvar kb-tms-next-id 0
  "Counter for generating unique justification IDs.")

;;; Core TMS functions

(defun kb-tms-init ()
  "Initialize the Truth Maintenance System."
  (setq kb-tms-facts (make-hash-table :test 'equal))
  (setq kb-tms-justifications (make-hash-table :test 'equal))
  (setq kb-tms-next-id 0))

(defun kb-tms-fact-key (subject predicate object &optional microtheory)
  "Generate a unique key for a fact."
  (list subject predicate object (or microtheory 'BaseMt)))

(defun kb-tms-get-fact-record (subject predicate object &optional microtheory)
  "Get or create a fact record."
  (let ((key (kb-tms-fact-key subject predicate object microtheory)))
    (or (gethash key kb-tms-facts)
        (let ((record (make-kb-fact-record
                      :subject subject
                      :predicate predicate
                      :object object
                      :microtheory (or microtheory 'BaseMt)
                      :justifications nil
                      :dependents nil
                      :belief-status 'unknown)))
          (puthash key record kb-tms-facts)
          record))))

(defun kb-tms-add-justification (fact-record premises rule support-type)
  "Add a justification to a fact record."
  (let* ((just-id (cl-incf kb-tms-next-id))
         (justification (make-kb-justification
                        :fact fact-record
                        :premises premises
                        :rule rule
                        :support-type support-type
                        :timestamp (current-time)
                        :active-p t)))
    
    ;; Add to justifications table
    (puthash just-id justification kb-tms-justifications)
    
    ;; Add to fact record
    (push justification (kb-fact-record-justifications fact-record))
    
    ;; Update dependency links
    (dolist (premise premises)
      (when premise
        (let ((premise-record (kb-tms-get-fact-record 
                              (nth 0 premise) (nth 1 premise) (nth 2 premise))))
          (push fact-record (kb-fact-record-dependents premise-record)))))
    
    ;; Update belief status
    (kb-tms-update-belief-status fact-record)
    
    just-id))

(defun kb-tms-update-belief-status (fact-record)
  "Update the belief status of a fact based on its justifications."
  (let ((active-justs (cl-remove-if-not 
                       (lambda (j) (kb-justification-active-p j))
                       (kb-fact-record-justifications fact-record))))
    
    (setf (kb-fact-record-belief-status fact-record)
          (if active-justs 'in 'out))
    
    ;; Propagate changes to dependents
    (when (eq (kb-fact-record-belief-status fact-record) 'out)
      (dolist (dependent (kb-fact-record-dependents fact-record))
        (kb-tms-check-dependent-justifications dependent fact-record)))))

(defun kb-tms-check-dependent-justifications (dependent withdrawn-premise)
  "Check if dependent facts are still justified after premise withdrawal."
  (dolist (just (kb-fact-record-justifications dependent))
    (when (kb-justification-active-p just)
      (dolist (premise (kb-justification-premises just))
        (when premise
          (let ((premise-record (kb-tms-get-fact-record 
                                (nth 0 premise) (nth 1 premise) (nth 2 premise))))
            (when (eq premise-record withdrawn-premise)
              ;; This justification depends on withdrawn premise
              (setf (kb-justification-active-p just) nil)))))))
  
  ;; Update belief status after checking justifications
  (kb-tms-update-belief-status dependent))

(defun kb-tms-retract-fact (subject predicate object &optional microtheory)
  "Retract a fact and update dependent facts."
  (let ((fact-record (kb-tms-get-fact-record subject predicate object microtheory)))
    (when fact-record
      ;; Deactivate all justifications for this fact
      (dolist (just (kb-fact-record-justifications fact-record))
        (setf (kb-justification-active-p just) nil))
      
      ;; Update belief status and propagate
      (kb-tms-update-belief-status fact-record))))

(defun kb-tms-assert-fact (subject predicate object &optional microtheory premises rule support-type)
  "Assert a fact with TMS tracking."
  (let ((fact-record (kb-tms-get-fact-record subject predicate object microtheory))
        (support-type (or support-type 'direct)))
    
    ;; Add justification
    (kb-tms-add-justification fact-record premises rule support-type)
    
    fact-record))

(defun kb-tms-is-believed (subject predicate object &optional microtheory)
  "Check if a fact is currently believed."
  (let ((fact-record (gethash (kb-tms-fact-key subject predicate object microtheory)
                             kb-tms-facts)))
    (and fact-record
         (eq (kb-fact-record-belief-status fact-record) 'in))))

(defun kb-tms-get-justification (fact-record)
  "Get the active justification for a fact."
  (cl-find-if (lambda (j) (kb-justification-active-p j))
              (kb-fact-record-justifications fact-record)))

(defun kb-tms-explain-fact (subject predicate object &optional microtheory)
  "Get explanation (justification chain) for a fact."
  (let ((fact-record (gethash (kb-tms-fact-key subject predicate object microtheory)
                             kb-tms-facts)))
    (when fact-record
      (kb-tms-build-explanation fact-record))))

(defun kb-tms-build-explanation (fact-record &optional visited)
  "Build explanation tree for a fact record."
  (let ((visited (or visited (make-hash-table :test 'equal)))
        (key (kb-tms-fact-key (kb-fact-record-subject fact-record)
                             (kb-fact-record-predicate fact-record)
                             (kb-fact-record-object fact-record)
                             (kb-fact-record-microtheory fact-record))))
    
    ;; Avoid cycles
    (when (gethash key visited)
      (return-from kb-tms-build-explanation '(CYCLE)))
    
    (puthash key t visited)
    
    (let ((active-just (kb-tms-get-justification fact-record)))
      (if active-just
          (let ((explanation 
                 (list :fact (list (kb-fact-record-subject fact-record)
                                  (kb-fact-record-predicate fact-record)
                                  (kb-fact-record-object fact-record))
                       :support-type (kb-justification-support-type active-just)
                       :rule (kb-justification-rule active-just))))
            
            ;; Add premise explanations
            (when (kb-justification-premises active-just)
              (setq explanation 
                    (append explanation
                            (list :premises 
                                  (mapcar (lambda (premise)
                                           (when premise
                                             (let ((premise-record 
                                                    (kb-tms-get-fact-record 
                                                     (nth 0 premise) (nth 1 premise) (nth 2 premise))))
                                               (kb-tms-build-explanation premise-record visited))))
                                         (kb-justification-premises active-just))))))
            explanation)
        :no-justification))))

;;; Interface functions

(defun kb-tms-status ()
  "Return TMS status information."
  (let ((total-facts (hash-table-count kb-tms-facts))
        (believed-facts 0)
        (total-justifications (hash-table-count kb-tms-justifications)))
    
    (maphash (lambda (key fact-record)
               (when (eq (kb-fact-record-belief-status fact-record) 'in)
                 (cl-incf believed-facts)))
             kb-tms-facts)
    
    (format "TMS Status: %d total facts, %d believed, %d justifications"
            total-facts believed-facts total-justifications)))

(defun kb-tms-validate ()
  "Validate TMS consistency."
  (let ((errors nil))
    
    ;; Check that all believed facts have active justifications
    (maphash (lambda (key fact-record)
               (when (eq (kb-fact-record-belief-status fact-record) 'in)
                 (unless (kb-tms-get-justification fact-record)
                   (push (format "Believed fact without justification: %s" key) errors))))
             kb-tms-facts)
    
    ;; Check dependency consistency
    (maphash (lambda (key fact-record)
               (dolist (dependent (kb-fact-record-dependents fact-record))
                 (let ((dependent-key (kb-tms-fact-key 
                                      (kb-fact-record-subject dependent)
                                      (kb-fact-record-predicate dependent)
                                      (kb-fact-record-object dependent)
                                      (kb-fact-record-microtheory dependent))))
                   (unless (gethash dependent-key kb-tms-facts)
                     (push (format "Dangling dependency: %s -> %s" key dependent-key) errors)))))
             kb-tms-facts)
    
    (if errors
        (cons 'errors errors)
      'valid)))

(provide 'kb-tms)

;;; kb-tms.el ends here
