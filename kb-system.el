;;; kb-system.el --- Knowledge Base System DSL (Legacy Interface)

;; Author: AI Assistant
;; Keywords: ai, knowledge base, ontology
;; Version: 2.0

;;; Commentary:

;; This package provides backward compatibility for the original knowledge base
;; system. For new development, use kb-advanced-system.el which provides the full
;; advanced functionality including microtheories, layered inference,
;; non-monotonic reasoning, and event reification.

;;; Code:

(require 'cl-lib)
(require 'kb-advanced-system)

;;; Custom Types

(cl-defstruct (kb-fact (:constructor kb-fact-create)
                       (:copier nil))
  "A structure representing a fact in the knowledge base."
  subject predicate object certainty)

(cl-defstruct (kb-rule (:constructor kb-rule-create)
                       (:copier nil))
  "A structure representing an inference rule."
  name premises conclusion)

;;; Variables

(defvar kb-system-knowledge-base (make-hash-table :test 'equal)
  "The main knowledge base hash table.")

(defvar kb-system-rules nil
  "List of inference rules.")

(defvar kb-system-ontology (make-hash-table :test 'equal)
  "Ontology hash table for storing class hierarchies.")

;;; Core Functions

(defun kb-system-add-fact (subject predicate object &optional certainty)
  "Add a fact to the knowledge base (legacy function)."
  (with-kb
    (kb-assert subject predicate object certainty)))

(defun kb-system-query (subject predicate)
  "Query the knowledge base for facts matching the subject and predicate (legacy function)."
  (with-kb
    (kb-query subject predicate)))

(defun kb-system-add-rule (name premises conclusion)
  "Add an inference rule to the system."
  (let ((rule (kb-rule-create :name name
                              :premises premises
                              :conclusion conclusion)))
    (push rule kb-system-rules)))

(defun kb-system-infer ()
  "Apply inference rules to derive new facts (legacy function)."
  (with-kb
    (kb-reason)))

(defun kb-system-match-premises (premises)
         "Match premises against the knowledge base, returning possible bindings."
         (let ((bindings '()))
           (dolist (premise premises)
             (let* ((subject (car premise))
                    (predicate (cadr premise))
                    (results (kb-system-query subject predicate)))
               (dolist (fact results)
                 (push (list subject predicate (kb-fact-object fact)) bindings)))))
         bindings)

(defun kb-system-apply-bindings (conclusion bindings)
  "Apply variable bindings to the conclusion of a rule.
CONCLUSION is a list representing the conclusion with variables.
BINDINGS is a list of pairs where each pair consists of a variable and its value."
  (let ((result conclusion))
    (dolist (binding bindings)
      (let ((var (car binding))
            (value (cadr binding)))
        ;; Replace each occurrence of var in result with value
        (setq result (mapcar (lambda (item)
                               (if (eq item var) value item))
                             result))))
    result))

;;; Ontology Functions

(defun kb-system-add-class (class &optional parent)
  "Add a class to the ontology, optionally specifying a parent class."
  (let ((parents (if parent (list parent) nil)))
    (puthash class parents kb-system-ontology)))

(defun kb-system-add-subclass (subclass superclass)
  "Add a subclass relationship to the ontology."
  (let ((parents (gethash subclass kb-system-ontology)))
    (puthash subclass (cons superclass parents) kb-system-ontology)))

(defun kb-system-is-a (subclass superclass)
  "Check if subclass is a descendant of superclass in the ontology."
  (or (eq subclass superclass)
      (let ((parents (gethash subclass kb-system-ontology)))
        (cl-some (lambda (parent) (kb-system-is-a parent superclass)) parents))))

;;; Query Language

(defmacro kb-system-with-query (&rest body)
  "Provide a DSL for querying the knowledge base."
  `(let ((result nil))
     ,@(mapcar (lambda (expr)
                 (pcase expr
                   (`(select ,subject ,predicate)
                    `(setq result (kb-system-query ',subject ',predicate)))
                   (`(where ,condition)
                    `(setq result (cl-remove-if-not (lambda (fact) ,condition) result)))
                   (`(order-by ,key)
                    `(setq result (sort result (lambda (a b) (< (,key a) (,key b))))))))
               body)
     result))

;;; Utility Functions

(defun kb-system-print-kb ()
  "Print the entire knowledge base."
  (maphash (lambda (k v)
             (princ (format "Subject: %s\n" k))
             (dolist (fact v)
               (princ (format "  %s\n" (kb-fact-object fact)))))
           kb-system-knowledge-base))

(defun kb-system-save-kb (filename)
  "Save the knowledge base to a file."
  (with-temp-file filename
    (prin1 kb-system-knowledge-base (current-buffer))))

(defun kb-system-load-kb (filename)
  "Load the knowledge base from a file."
  (setq kb-system-knowledge-base
        (with-temp-buffer
          (insert-file-contents filename)
          (read (current-buffer)))))

;;; Legacy Compatibility Notice

;; The following example usage is preserved for backward compatibility.
;; For new projects, please use the enhanced kb-advanced-system.el API.

;;; Example Usage (Legacy - use kb-demo for new examples)

;; (kb-system-add-class 'animal)
;; (kb-system-add-class 'mammal 'animal)
;; (kb-system-add-class 'human 'mammal)

;; (kb-system-add-fact 'Socrates 'is-a 'human)
;; (kb-system-add-fact 'human 'is-mortal t)

;; (kb-system-add-rule 'mortality-rule
;;                     '(((?x is-a ?y) (?y is-mortal t)))
;;                     '(?x is-mortal t))

;; (kb-system-infer)

;; For enhanced queries, use: (kb-demo)

(provide 'kb-system)
;;; kb-system.el ends here

