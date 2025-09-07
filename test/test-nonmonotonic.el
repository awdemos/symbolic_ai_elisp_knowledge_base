;;; test-nonmonotonic.el --- Non-monotonic reasoning tests

;;; Commentary:
;; Tests for default rules, exceptions, and belief revision.

;;; Code:

(require 'ert)
(require 'kb-advanced-system)

(ert-deftest test-default-rule ()
  "Test basic default rule application."
  (kb-init)
  
  ;; Add default rule: birds can fly
  (kb-add-default 'birds-fly '((?x is-a bird)) '(?x can-fly t))
  
  (kb-assert 'Tweety 'is-a 'bird)
  (kb-reason)
  
  (should (member t (kb-query 'Tweety 'can-fly))))

(ert-deftest test-exception-overrides-default ()
  "Test that exceptions override defaults."
  (kb-init)
  
  ;; Default: birds can fly
  (kb-add-default 'birds-fly '((?x is-a bird)) '(?x can-fly t))
  
  ;; Exception: penguins cannot fly
  (kb-add-exception 'penguin-exception 'birds-fly 
                    '((?x is-a penguin)) '(?x can-fly nil))
  
  (kb-assert 'Pingu 'is-a 'penguin)
  (kb-assert 'Pingu 'is-a 'bird)  ; penguins are birds
  (kb-reason)
  
  ;; Exception should override default
  (should (member nil (kb-query 'Pingu 'can-fly))))

(ert-deftest test-multiple-defaults ()
  "Test multiple default rules."
  (kb-init)
  
  ;; Birds can fly
  (kb-add-default 'birds-fly '((?x is-a bird)) '(?x can-fly t))
  
  ;; Mammals are warm-blooded
  (kb-add-default 'mammals-warm '((?x is-a mammal)) '(?x warm-blooded t))
  
  (kb-assert 'Robin 'is-a 'bird)
  (kb-assert 'Dog 'is-a 'mammal)
  (kb-reason)
  
  (should (member t (kb-query 'Robin 'can-fly)))
  (should (member t (kb-query 'Dog 'warm-blooded))))

(ert-deftest test-conflicting-defaults ()
  "Test handling of conflicting defaults."
  (kb-init)
  
  ;; Quakers are pacifists
  (kb-add-default 'quaker-pacifist '((?x is-a quaker)) '(?x pacifist t))
  
  ;; Republicans are not pacifists  
  (kb-add-default 'republican-not-pacifist '((?x is-a republican)) '(?x pacifist nil))
  
  (kb-assert 'Nixon 'is-a 'quaker)
  (kb-assert 'Nixon 'is-a 'republican)
  (kb-reason)
  
  ;; Should handle conflict gracefully (most systems prefer specificity or recency)
  (let ((result (kb-query 'Nixon 'pacifist)))
    (should result))) ; Just ensure it doesn't crash

;;; test-nonmonotonic.el ends here
