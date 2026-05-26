;;; kb-query-engine.el --- Query engine for Knowledge Base
;; -*- lexical-binding: t; -*-

;; Author: AI Assistant
;; Keywords: ai, knowledge base, query, inheritance
;; Version: 1.0

;;; Commentary:

;; This package provides the query engine for the Knowledge Base system,
;; handling fact retrieval, inheritance resolution, conflict resolution,
;; and temporal queries.

;;; Code:

(require 'cl-lib)
(require 'kb-structs)
(require 'kb-tms)

;; Forward declarations to avoid circular dependency with kb-microtheories
(declare-function kb-get-microtheory "kb-microtheories" (name))
(declare-function kb-get-inheritance-chain "kb-microtheories" (mt-name))
(declare-function kb-get-microtheory-priority "kb-microtheories" (mt-name))
(declare-function kb-clear-inheritance-cache "kb-microtheories" ())

;;; Variables

(defvar kb-inheritance-cache (make-hash-table :test 'equal)
  "Cache for inheritance chains to improve performance.")

;;; Core Query Functions

(defun kb-query-in-mt (subject predicate mt-name)
  "Query facts in a specific microtheory MT-NAME.
Returns a list of kb-fact structures matching SUBJECT and PREDICATE."
  (let ((mt (kb-get-microtheory mt-name)))
    (when mt
      (let ((facts (gethash subject (kb-microtheory-facts mt))))
        (cl-remove-if-not
         (lambda (fact)
           (and (eq (kb-fact-predicate fact) predicate)
                (> (kb-fact-certainty fact) 0)))
         facts)))))

(defun kb-query-with-inheritance (subject predicate &optional mt-name)
  "Query facts with proper microtheory inheritance and shadowing.
Searches MT-NAME and its ancestor microtheories, applying shadowing
rules based on the inheritance mode."
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
  "Resolve conflicts between inherited facts based on inheritance mode.
Groups facts by object value and applies the resolution strategy
of the microtheory MT-NAME."
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
  "Apply conflict resolution strategy to a group of conflicting facts.
MODE can be 'strict, 'override, or 'merge (default)."
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
  "Query facts with inheritance, filtering by TMS belief status.
Only returns facts that the TMS considers currently believed."
  (let ((candidates (kb-query-with-inheritance subject predicate mt-name)))
    (cl-remove-if-not 
     (lambda (fact)
       (kb-tms-is-believed (kb-fact-subject fact)
                          (kb-fact-predicate fact)
                          (kb-fact-object fact)
                          (kb-fact-microtheory fact)))
     candidates)))

;;; Temporal Query Functions

(defun kb-query-at-time (subject predicate time &optional mt-name)
  "Query facts that are valid at a specific TIME.
Only returns facts whose temporal validity includes TIME."
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

;;; Utility Functions

(defun kb-fact-equal-p (fact1 fact2)
  "Check if two facts are equal.
Compares subject, predicate, and object fields."
  (and (eq (kb-fact-subject fact1) (kb-fact-subject fact2))
       (eq (kb-fact-predicate fact1) (kb-fact-predicate fact2))
       (equal (kb-fact-object fact1) (kb-fact-object fact2))))

(provide 'kb-query-engine)
;;; kb-query-engine.el ends here
