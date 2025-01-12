;;; kb-nonmonotonic.el --- Non-monotonic Reasoning for Knowledge Base

;; Author: AI Assistant
;; Keywords: ai, nonmonotonic, defaults, exceptions
;; Version: 2.0

;;; Commentary:

;; This package implements non-monotonic reasoning capabilities including
;; default logic, exception handling, and belief revision for advanced reasoning.

;;; Code:

(require 'cl-lib)
(require 'kb-microtheories)

;;; Non-monotonic Structures

(cl-defstruct (kb-default-rule (:constructor kb-default-rule-create)
                               (:copier nil))
  "A default rule that can be overridden."
  name
  premises              ; conditions that must hold
  conclusion           ; what to conclude by default
  exceptions           ; list of exception conditions
  strength             ; strength of the default (0.0 - 1.0)
  microtheory
  specificity)         ; how specific this rule is

(cl-defstruct (kb-exception (:constructor kb-exception-create)
                            (:copier nil))
  "An exception to a default rule."
  name
  rule-name            ; which default rule this excepts
  conditions           ; when this exception applies
  alternative          ; what to conclude instead (optional)
  priority             ; priority of this exception
  microtheory)

(cl-defstruct (kb-justification (:constructor kb-justification-create)
                                (:copier nil))
  "Justification for why a fact is believed."
  type                 ; :default, :rule, :fact, :exception
  source               ; the rule/fact that justified this
  dependencies         ; facts this justification depends on
  strength             ; confidence in this justification
  defeated-by          ; what defeats this justification (if any)
  active-p)            ; whether this justification is currently active

;;; Variables

(defvar kb-default-rules nil
  "List of default rules.")

(defvar kb-exceptions nil
  "List of exceptions to default rules.")

(defvar kb-justifications (make-hash-table :test 'equal)
  "Hash table mapping facts to their justifications.")

(defvar kb-defeated-justifications nil
  "List of currently defeated justifications.")

;;; Default Rule Management

(defun kb-add-default-rule (name premises conclusion &optional exceptions strength specificity)
  "Add a default rule to the knowledge base."
  (let ((rule (kb-default-rule-create
               :name name
               :premises premises
               :conclusion conclusion
               :exceptions (or exceptions nil)
               :strength (or strength 0.8)
               :microtheory kb-current-mt
               :specificity (or specificity 1))))
    (push rule kb-default-rules)
    rule))

(defun kb-add-exception (name rule-name conditions &optional alternative priority)
  "Add an exception to a default rule."
  (let ((exception (kb-exception-create
                   :name name
                   :rule-name rule-name
                   :conditions conditions
                   :alternative alternative
                   :priority (or priority 1.0)
                   :microtheory kb-current-mt)))
    (push exception kb-exceptions)
    
    ;; Update the default rule to include this exception
    (let ((rule (cl-find rule-name kb-default-rules 
                        :key #'kb-default-rule-name)))
      (when rule
        (push name (kb-default-rule-exceptions rule))))
    exception))

;;; Non-monotonic Inference

(defun kb-apply-default-rules (&optional mt-name)
  "Apply default rules with exception handling."
  (let* ((mt-name (or mt-name kb-current-mt))
         (applicable-rules (kb-get-applicable-defaults mt-name))
         (new-beliefs nil))
    
    ;; Sort rules by specificity (more specific first)
    (setq applicable-rules 
          (sort applicable-rules 
                (lambda (r1 r2) 
                  (> (kb-default-rule-specificity r1)
                     (kb-default-rule-specificity r2)))))
    
    (dolist (rule applicable-rules)
      (let ((bindings (kb-match-default-premises rule mt-name)))
        (dolist (binding bindings)
          (unless (kb-check-exceptions rule binding mt-name)
            (let* ((conclusion (kb-apply-bindings 
                               (kb-default-rule-conclusion rule) binding))
                   (new-fact (kb-create-default-fact conclusion rule binding)))
              (push new-fact new-beliefs))))))
    
    ;; Add new beliefs and their justifications
    (dolist (belief new-beliefs)
      (kb-add-justified-fact belief))
    
    new-beliefs))

(defun kb-get-applicable-defaults (mt-name)
  "Get default rules applicable in microtheory."
  (cl-remove-if-not 
   (lambda (rule)
     (or (eq (kb-default-rule-microtheory rule) mt-name)
         (kb-inherits-from-mt-p mt-name (kb-default-rule-microtheory rule))))
   kb-default-rules))

(defun kb-inherits-from-mt-p (mt-name parent-mt)
  "Check if mt-name inherits from parent-mt."
  (let ((mt (kb-get-microtheory mt-name)))
    (when mt
      (member parent-mt (kb-microtheory-inherits-from mt)))))

(defun kb-match-default-premises (rule mt-name)
  "Match premises of a default rule."
  (if (null (kb-default-rule-premises rule))
      (list nil)  ; No premises - always applicable
    (kb-match-premises (kb-default-rule-premises rule) mt-name)))

(defun kb-check-exceptions (rule binding mt-name)
  "Check if any exceptions defeat this default rule application."
  (cl-some 
   (lambda (exception-name)
     (let ((exception (cl-find exception-name kb-exceptions 
                              :key #'kb-exception-name)))
       (when exception
         (kb-exception-applies-p exception binding mt-name))))
   (kb-default-rule-exceptions rule)))

(defun kb-exception-applies-p (exception binding mt-name)
  "Check if an exception applies given the binding."
  (let ((conditions (kb-exception-conditions exception)))
    (cl-every 
     (lambda (condition)
       (let ((instantiated (kb-apply-bindings condition (list binding))))
         (kb-query-with-inheritance 
          (car instantiated) (cadr instantiated) mt-name)))
     conditions)))

(defun kb-create-default-fact (conclusion rule binding)
  "Create a fact with default justification."
  (let ((fact (kb-fact-create
               :subject (car conclusion)
               :predicate (cadr conclusion)
               :object (caddr conclusion)
               :certainty (kb-default-rule-strength rule)
               :microtheory (kb-default-rule-microtheory rule)))
        (justification (kb-justification-create
                       :type :default
                       :source rule
                       :dependencies binding
                       :strength (kb-default-rule-strength rule)
                       :active-p t)))
    (puthash (kb-fact-signature fact) justification kb-justifications)
    fact))

(defun kb-fact-signature (fact)
  "Create a unique signature for a fact."
  (list (kb-fact-subject fact) 
        (kb-fact-predicate fact)
        (kb-fact-object fact)
        (kb-fact-microtheory fact)))

(defun kb-add-justified-fact (fact)
  "Add a fact with its justification to the knowledge base."
  (kb-with-microtheory (kb-fact-microtheory fact)
    (kb-add-fact (kb-fact-subject fact)
                 (kb-fact-predicate fact)
                 (kb-fact-object fact)
                 (kb-fact-certainty fact)
                 (kb-fact-temporal-info fact))))

;;; Belief Revision

(defun kb-add-contradictory-fact (subject predicate object &optional certainty)
  "Add a fact that may contradict existing beliefs."
  (let* ((new-fact (kb-fact-create
                   :subject subject
                   :predicate predicate
                   :object object
                   :certainty (or certainty 1.0)
                   :microtheory kb-current-mt))
         (conflicts (kb-find-conflicts new-fact)))
    
    ;; Handle conflicts through belief revision
    (when conflicts
      (kb-revise-beliefs new-fact conflicts))
    
    ;; Add the new fact
    (kb-add-justified-fact new-fact)
    new-fact))

(defun kb-find-conflicts (new-fact)
  "Find existing facts that conflict with the new fact."
  (let ((conflicts nil)
        (subject (kb-fact-subject new-fact))
        (predicate (kb-fact-predicate new-fact))
        (new-object (kb-fact-object new-fact)))
    
    ;; Look for contradictory facts
    (let ((existing-facts (kb-query-with-inheritance 
                          subject predicate (kb-fact-microtheory new-fact))))
      (dolist (fact existing-facts)
        (when (and (not (equal (kb-fact-object fact) new-object))
                  (kb-contradictory-p fact new-fact))
          (push fact conflicts))))
    
    conflicts))

(defun kb-contradictory-p (fact1 fact2)
  "Check if two facts are contradictory."
  (and (eq (kb-fact-subject fact1) (kb-fact-subject fact2))
       (eq (kb-fact-predicate fact1) (kb-fact-predicate fact2))
       (not (equal (kb-fact-object fact1) (kb-fact-object fact2)))
       ;; Add more sophisticated contradiction detection here
       (kb-mutually-exclusive-p (kb-fact-object fact1) (kb-fact-object fact2))))

(defun kb-mutually-exclusive-p (obj1 obj2)
  "Check if two objects are mutually exclusive."
  ;; Simple cases - can be extended with ontological knowledge
  (cond
   ((and (booleanp obj1) (booleanp obj2)) (not (eq obj1 obj2)))
   ((and (numberp obj1) (numberp obj2)) nil)  ; Numbers can coexist
   (t nil)))  ; Default: not mutually exclusive

(defun kb-revise-beliefs (new-fact conflicts)
  "Revise beliefs when conflicts are detected."
  (dolist (conflict conflicts)
    (let* ((signature (kb-fact-signature conflict))
           (justification (gethash signature kb-justifications)))
      
      (when justification
        (cond
         ;; If conflicting fact has weaker justification, defeat it
         ((< (kb-justification-strength justification)
             (kb-fact-certainty new-fact))
          (kb-defeat-justification justification conflict new-fact))
         
         ;; If new fact is weaker, it might not be added
         ((> (kb-justification-strength justification)
             (kb-fact-certainty new-fact))
          (message "Warning: New fact %s conflicts with stronger belief"
                  (kb-fact-signature new-fact)))
         
         ;; Equal strength - mark both as uncertain
         (t
          (kb-mark-uncertain justification conflict new-fact)))))))

(defun kb-defeat-justification (justification old-fact new-fact)
  "Defeat a justification, making the old fact no longer believed."
  (setf (kb-justification-active-p justification) nil)
  (setf (kb-justification-defeated-by justification) new-fact)
  (push justification kb-defeated-justifications)
  
  ;; Remove the old fact from the knowledge base
  (kb-remove-fact old-fact))

(defun kb-mark-uncertain (justification old-fact new-fact)
  "Mark both facts as uncertain due to conflict."
  (setf (kb-justification-strength justification) 0.5)
  ;; Could add more sophisticated uncertainty handling here
  )

(defun kb-remove-fact (fact)
  "Remove a fact from the knowledge base."
  (let* ((mt (kb-get-microtheory (kb-fact-microtheory fact)))
         (subject (kb-fact-subject fact))
         (facts (gethash subject (kb-microtheory-facts mt))))
    (when facts
      (puthash subject 
               (cl-remove fact facts :test #'kb-facts-equal-p)
               (kb-microtheory-facts mt)))))

(defun kb-facts-equal-p (fact1 fact2)
  "Check if two facts are exactly equal."
  (and (eq (kb-fact-subject fact1) (kb-fact-subject fact2))
       (eq (kb-fact-predicate fact1) (kb-fact-predicate fact2))
       (equal (kb-fact-object fact1) (kb-fact-object fact2))))

;;; Truth Maintenance

(defun kb-restore-defeated-justifications ()
  "Attempt to restore defeated justifications if their defeaters are gone."
  (let ((restored nil))
    (dolist (justification kb-defeated-justifications)
      (let ((defeater (kb-justification-defeated-by justification)))
        (unless (kb-fact-still-believed-p defeater)
          ;; Defeater is gone, restore this justification
          (setf (kb-justification-active-p justification) t)
          (setf (kb-justification-defeated-by justification) nil)
          (push justification restored))))
    
    ;; Remove restored justifications from defeated list
    (dolist (restored-just restored)
      (setq kb-defeated-justifications 
            (delq restored-just kb-defeated-justifications)))
    
    restored))

(defun kb-fact-still-believed-p (fact)
  "Check if a fact is still believed in the knowledge base."
  (let ((current-facts (kb-query-with-inheritance 
                       (kb-fact-subject fact)
                       (kb-fact-predicate fact)
                       (kb-fact-microtheory fact))))
    (cl-member fact current-facts :test #'kb-facts-equal-p)))

;;; High-level Interface

(defun kb-reason-with-defaults (&optional mt-name)
  "Perform complete default reasoning in microtheory."
  (let ((mt-name (or mt-name kb-current-mt)))
    ;; Apply defaults
    (let ((new-beliefs (kb-apply-default-rules mt-name)))
      ;; Check for any inconsistencies and revise
      (kb-restore-defeated-justifications)
      new-beliefs)))

;;; Example Usage and Setup

(defun kb-setup-default-reasoning-examples ()
  "Set up example default rules and exceptions."
  
  ;; Birds typically fly
  (kb-add-default-rule 'birds-fly
                       '((?x is-a bird))
                       '(?x can-fly t)
                       nil 0.9 1)
  
  ;; Exception: penguins are birds but don't fly
  (kb-add-exception 'penguin-exception 
                    'birds-fly
                    '((?x is-a penguin))
                    '(?x can-fly nil)
                    2.0)
  
  ;; Mammals are typically warm-blooded
  (kb-add-default-rule 'mammals-warm-blooded
                       '((?x is-a mammal))
                       '(?x warm-blooded t)
                       nil 0.95 1)
  
  ;; Students are typically young
  (kb-add-default-rule 'students-young
                       '((?x is-a student))
                       '(?x age-category young)
                       nil 0.7 1))

(provide 'kb-nonmonotonic)
;;; kb-nonmonotonic.el ends here
