;;; kb-system.el --- Knowledge Base System DSL (Legacy Interface)
;; -*- lexical-binding: t; -*-

;; Author: AI Assistant
;; Keywords: ai, knowledge base, ontology
;; Version: 2.0

;;; Commentary:

;; This package provides backward compatibility for the original knowledge base
;; system. For new development, use kb-advanced-system.el which provides
;; the full interface including microtheories, layered inference, non-monotonic
;; reasoning, temporal logic, event reification, debugging tools, query caching,
;; testing framework, RDF/OWL import, and database persistence.

;; The legacy interface supports a simple fact-query-assert API:
;; - kb-system-assert: Add facts
;; - kb-system-query: Query for facts
;; - kb-system-add-rule: Add inference rules
;; - kb-system-match-premises: Match rule premises

;;; Code:

(require 'cl-lib)

;;; Structures

(cl-defstruct (kb-system-fact (:constructor kb-system-fact-create)
                            (:copier nil))
  subject
  predicate
  object)

(cl-defstruct (kb-system-rule (:constructor kb-system-rule-create)
                             (:copier nil))
  name
  premises
  conclusion)

;;; Variables

(defvar kb-system-facts nil
  "Hash table storing all facts in the system.
Keys are (subject predicate) pairs, values are lists of objects.")

(defvar kb-system-rules nil
  "List of all inference rules in the system.")

;;; API Functions

;;;###autoload
(defun kb-system-assert (subject predicate object)
  "Assert a fact into the knowledge base.
SUBJECT, PREDICATE and OBJECT define the fact to add."
  (interactive "sAssert fact (s p o): ")
  (unless kb-system-facts
    (setq kb-system-facts (make-hash-table :test 'equal)))
  (let ((key (cons subject predicate))
        (facts (gethash key kb-system-facts)))
    (puthash key (cons object facts) kb-system-facts)
    (message "Asserted: (%s %s %s)" subject predicate object)))

;;;###autoload
(defun kb-system-query (subject predicate)
  "Query the knowledge base for facts matching SUBJECT and PREDICATE.
Returns a list of all objects matching the query."
  (interactive "sQuery facts (s p): ")
  (when kb-system-facts
    (let ((key (cons subject predicate)))
      (gethash key kb-system-facts))))

;;;###autoload
(defun kb-system-add-rule (name premises conclusion)
  "Add an inference rule to the system.
NAME identifies the rule.
PREMISES are the conditions that must hold.
CONCLUSION is what can be inferred when premises are true."
  (interactive "sAdd rule (s): \nNew rule name: ")
  (push (kb-system-rule-create
         :name name
         :premises premises
         :conclusion conclusion)
        kb-system-rules)
  (message "Added rule: %s" name))

(defun kb-system-match-premises (premises)
  "Match premises against knowledge in the base, returning possible bindings.
PREMISES is a list of (subject predicate object) patterns or variables.
Returns a list of binding lists, where each binding is a list of (variable . value) pairs."
  (let ((bindings '(())))  ; Start with empty binding
    (dolist (premise premises)
      (let* ((subject (car premise))
             (predicate (cadr premise))
             (object (caddr premise))
             (new-bindings '()))
        ;; If premise has variables, try to match
        (if (or (and (symbolp subject) (string-match-p "^?" (symbol-name subject)))
                (and (symbolp predicate) (string-match-p "^?" (symbol-name predicate)))
                (and (symbolp object) (string-match-p "^?" (symbol-name object)))
            ;; Pattern matching with variables
            (let* ((all-facts (when kb-system-facts
                                   (kb-system-get-all-facts)))
                   (variable-subject (if (and (symbolp subject) (string-match-p "^?" (symbol-name subject)))
                                   subject
                                 nil))
                   (variable-predicate (if (and (symbolp predicate) (string-match-p "^?" (symbol-name predicate)))
                                     predicate
                                     nil))
                   (variable-object (if (and (symbolp object) (string-match-p "^?" (symbol-name object)))
                                   object
                                    nil)))
              ;; Try to bind variables to facts
              (dolist (fact all-facts)
                (let ((new-binding nil))
                  ;; Check if fact matches pattern
                  (when (or (not variable-subject)
                              (equal (kb-system-fact-subject fact) variable-subject))
                              (not variable-predicate)
                              (equal (kb-system-fact-predicate fact) variable-predicate))
                              (not variable-object)
                              (equal (kb-system-fact-object fact) variable-object))
                    (setq new-binding t))
                  ;; Extend existing bindings
                  (dolist (binding bindings)
                    (when (every (lambda (var)
                                    (let ((value (cdr (assq var binding))))
                                      (or (null value)
                                          (and (symbolp subject) (string-match-p "^?" (symbol-name subject))
                                              (equal value variable-subject))
                                          (and (symbolp predicate) (string-match-p "^?" (symbol-name predicate))
                                              (equal value variable-predicate))
                                          (and (symbolp object) (string-match-p "^?" (symbol-name object))
                                              (equal value variable-object)))))
                                  (list variable-subject variable-predicate variable-object))
                              new-binding))
                      (push (cons (cons variable-subject (cons variable-predicate variable-object)) binding) new-bindings))))
              (setq bindings new-bindings)))
          ;; Constant premise - no matching needed
          bindings))
    bindings))

(defun kb-system-get-all-facts ()
  "Get all facts from the knowledge base."
  (let ((all-facts nil))
    (when kb-system-facts
      (maphash (lambda (key facts)
                  (dolist (fact facts)
                    (push fact all-facts)))
                kb-system-facts))
    (nreverse all-facts)))

(provide 'kb-system)
;;; kb-system.el ends here
