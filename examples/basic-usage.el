;;; basic-usage.el --- Basic usage examples for KB system

;;; Commentary:
;; This file demonstrates basic usage of the KB system.

;;; Code:

;; Load the system
(add-to-list 'load-path "./lisp")
(require 'kb-advanced-system)

;; Initialize
(kb-init)

;; Basic facts
(kb-assert 'Socrates 'is-a 'human)
(kb-assert 'human 'is-a 'mammal)

;; Query with inference
(kb-ask '(Socrates is-a))

;; Use microtheories
(kb-create-microtheory 'ScientificMt 'BaseMt)
(in-microtheory ScientificMt
  (kb-assert 'water 'chemical-formula 'H2O)
  (kb-assert 'water 'boiling-point 100))

;; Default reasoning with exceptions
(kb-add-default 'birds-fly 
                '((?x is-a bird)) 
                '(?x can-fly t))

(kb-add-exception 'penguin-exception 'birds-fly 
                  '((?x is-a penguin)) 
                  '(?x can-fly nil))

(kb-assert 'Tweety 'is-a 'bird)
(kb-assert 'Pingu 'is-a 'penguin)

;; Reason and query
(kb-reason)
(kb-query 'Tweety 'can-fly)  ; => t
(kb-query 'Pingu 'can-fly)   ; => nil

;; Check status
(kb-status)

;;; basic-usage.el ends here
