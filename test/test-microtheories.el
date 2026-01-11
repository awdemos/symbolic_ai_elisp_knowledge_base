;;; test-microtheories.el --- Microtheory system tests

;;; Commentary:
;; Tests for microtheory creation, inheritance, and context isolation.

;;; Code:

(require 'ert)
(require 'kb-advanced-system)

(ert-deftest test-create-microtheory ()
  "Test microtheory creation."
  (kb-init)
  (kb-create-microtheory 'TestMt '(BaseMt))
  (should (kb-get-microtheory 'TestMt)))

(ert-deftest test-microtheory-context-isolation ()
  "Test that microtheories isolate facts."
  (kb-init)
  (kb-create-microtheory 'Mt1 '(BaseMt))
  (kb-create-microtheory 'Mt2 '(BaseMt))

  (in-microtheory Mt1
    (kb-assert 'God 'exists t))

  (in-microtheory Mt2
    (kb-assert 'God 'exists nil))

  ;; Facts should be isolated
  (in-microtheory Mt1
    (should (equal t (kb-fact-object (car (kb-query 'God 'exists)))))

  (in-microtheory Mt2
    (should (equal nil (kb-fact-object (car (kb-query 'God 'exists))))))

(ert-deftest test-microtheory-inheritance ()
  "Test that child microtheories inherit from parents."
  (kb-init)
  (kb-create-microtheory 'ParentMt '(BaseMt))
  (kb-create-microtheory 'ChildMt '(ParentMt))

  (in-microtheory ParentMt
    (kb-assert 'base 'fact 'inherited))

  (in-microtheory ChildMt
    ;; Should inherit from parent
    (should (member 'inherited (mapcar #'kb-fact-object (kb-query 'base 'fact))))))

(ert-deftest test-multiple-microtheory-parents ()
  "Test microtheories with multiple parents."
  (kb-init)
  (kb-create-microtheory 'Parent1Mt '(BaseMt))
  (kb-create-microtheory 'Parent2Mt '(BaseMt))
  (kb-create-microtheory 'ChildMt '(Parent1Mt Parent2Mt))

  (in-microtheory Parent1Mt
    (kb-assert 'fact1 'source 'parent1))

  (in-microtheory Parent2Mt
    (kb-assert 'fact2 'source 'parent2))

  (in-microtheory ChildMt
    (should (member 'parent1 (mapcar #'kb-fact-object (kb-query 'fact1 'source))))
    (should (member 'parent2 (mapcar #'kb-fact-object (kb-query 'fact2 'source))))))

;;; test-microtheories.el ends here
