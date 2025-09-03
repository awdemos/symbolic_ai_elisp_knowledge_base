;;; kb-events.el --- Event Reification and Event-based Reasoning

;; Author: AI Assistant
;; Keywords: ai, events, reification, processes
;; Version: 2.0

;;; Commentary:

;; This package implements event reification for advanced reasoning,
;; treating events and processes as first-class objects that can be
;; reasoned about, related, and queried.

;;; Code:

(require 'cl-lib)
(require 'kb-microtheories)

;;; Event Structures

(cl-defstruct (kb-event (:constructor kb-event-create)
                        (:copier nil))
  "A reified event or process."
  id                    ; unique event identifier
  type                 ; type of event (e.g., walking, meeting)
  participants         ; list of entities involved
  roles                ; roles of participants (agent, patient, etc.)
  start-time
  end-time
  duration
  location
  properties           ; additional event properties
  sub-events           ; list of constituent events
  super-event          ; parent event this is part of
  causal-relations     ; events this causes/is caused by
  microtheory)

(cl-defstruct (kb-process (:constructor kb-process-create)
                          (:copier nil))
  "A process type (e.g., Walking, Eating)."
  name
  typical-duration
  typical-participants
  typical-roles
  preconditions        ; what must be true before
  effects              ; what becomes true after
  invariants           ; what remains true during
  microtheory)

(cl-defstruct (kb-event-relation (:constructor kb-event-relation-create)
                                 (:copier nil))
  "A relation between events."
  type                 ; :causes, :enables, :prevents, :part-of, :before, :after, :during
  source-event
  target-event
  strength             ; confidence in this relation
  microtheory)

;;; Variables

(defvar kb-events (make-hash-table :test 'equal)
  "Hash table of reified events.")

(defvar kb-processes (make-hash-table :test 'equal)
  "Hash table of process types.")

(defvar kb-event-relations nil
  "List of relations between events.")

(defvar kb-event-counter 0
  "Counter for generating unique event IDs.")

;;; Event Creation and Management

(defun kb-create-event (type &rest properties)
  "Create a new event instance."
  (let* ((event-id (intern (format "Event-%d" (cl-incf kb-event-counter))))
         (event (kb-event-create
                :id event-id
                :type type
                :participants (plist-get properties :participants)
                :roles (plist-get properties :roles)
                :start-time (plist-get properties :start-time)
                :end-time (plist-get properties :end-time)
                :duration (plist-get properties :duration)
                :location (plist-get properties :location)
                :properties properties
                :microtheory kb-current-mt)))
    
    (puthash event-id event kb-events)
    
    ;; Add facts about this event to the knowledge base
    (kb-add-event-facts event)
    
    event-id))

(defun kb-add-event-facts (event)
  "Add facts about an event to the knowledge base."
  (let ((event-id (kb-event-id event))
        (type (kb-event-type event)))
    
    ;; Basic event facts
    (kb-add-fact event-id 'is-a 'event)
    (kb-add-fact event-id 'event-type type)
    
    ;; Participant facts
    (when (kb-event-participants event)
      (dolist (participant (kb-event-participants event))
        (kb-add-fact event-id 'has-participant participant)))
    
    ;; Role facts
    (when (kb-event-roles event)
      (cl-loop for role in (kb-event-roles event) by #'cddr
               for participant in (cdr (kb-event-roles event)) by #'cddr
               do (kb-add-fact event-id role participant)))
    
    ;; Temporal facts
    (when (kb-event-start-time event)
      (kb-add-fact event-id 'starts-at (kb-event-start-time event)))
    
    (when (kb-event-end-time event)
      (kb-add-fact event-id 'ends-at (kb-event-end-time event)))
    
    (when (kb-event-duration event)
      (kb-add-fact event-id 'has-duration (kb-event-duration event)))
    
    ;; Location facts
    (when (kb-event-location event)
      (kb-add-fact event-id 'occurs-at (kb-event-location event)))))

(defun kb-get-event (event-id)
  "Get an event by its ID."
  (gethash event-id kb-events))

;;; Process Types

(defun kb-define-process (name &rest properties)
  "Define a new type of process."
  (let ((process (kb-process-create
                 :name name
                 :typical-duration (plist-get properties :typical-duration)
                 :typical-participants (plist-get properties :typical-participants)
                 :typical-roles (plist-get properties :typical-roles)
                 :preconditions (plist-get properties :preconditions)
                 :effects (plist-get properties :effects)
                 :invariants (plist-get properties :invariants)
                 :microtheory kb-current-mt)))
    
    (puthash name process kb-processes)
    
    ;; Add facts about this process type
    (kb-add-process-facts process)
    
    process))

(defun kb-add-process-facts (process)
  "Add facts about a process type to the knowledge base."
  (let ((name (kb-process-name process)))
    
    (kb-add-fact name 'is-a 'process-type)
    
    ;; Typical duration
    (when (kb-process-typical-duration process)
      (kb-add-fact name 'typical-duration (kb-process-typical-duration process)))
    
    ;; Preconditions
    (dolist (precond (kb-process-preconditions process))
      (kb-add-fact name 'precondition precond))
    
    ;; Effects
    (dolist (effect (kb-process-effects process))
      (kb-add-fact name 'effect effect))
    
    ;; Invariants
    (dolist (invariant (kb-process-invariants process))
      (kb-add-fact name 'invariant invariant))))

(defun kb-get-process (name)
  "Get a process type by name."
  (gethash name kb-processes))

;;; Event Relations

(defun kb-add-event-relation (type source-event target-event &optional strength)
  "Add a relation between two events."
  (let ((relation (kb-event-relation-create
                  :type type
                  :source-event source-event
                  :target-event target-event
                  :strength (or strength 1.0)
                  :microtheory kb-current-mt)))
    
    (push relation kb-event-relations)
    
    ;; Add facts about the relation
    (kb-add-fact source-event type target-event)
    
    ;; Add inverse relations where appropriate
    (let ((inverse (kb-get-inverse-relation type)))
      (when inverse
        (kb-add-fact target-event inverse source-event)))
    
    relation))

(defun kb-get-inverse-relation (relation-type)
  "Get the inverse of a relation type."
  (pcase relation-type
    (:before :after)
    (:after :before)
    (:causes :caused-by)
    (:caused-by :causes)
    (:enables :enabled-by)
    (:enabled-by :enables)
    (:part-of :has-part)
    (:has-part :part-of)
    (_ nil)))

;;; Event Reasoning

(defun kb-infer-event-relations ()
  "Infer additional event relations based on existing ones."
  (let ((new-relations nil))
    
    ;; Transitivity of temporal relations
    (dolist (rel1 kb-event-relations)
      (when (member (kb-event-relation-type rel1) '(:before :after))
        (dolist (rel2 kb-event-relations)
          (when (and (eq (kb-event-relation-type rel1) 
                        (kb-event-relation-type rel2))
                    (eq (kb-event-relation-target-event rel1)
                        (kb-event-relation-source-event rel2)))
            ;; rel1: A before B, rel2: B before C => A before C
            (let ((new-rel (kb-event-relation-create
                           :type (kb-event-relation-type rel1)
                           :source-event (kb-event-relation-source-event rel1)
                           :target-event (kb-event-relation-target-event rel2)
                           :strength (* (kb-event-relation-strength rel1)
                                      (kb-event-relation-strength rel2)
                                      0.9)  ; Confidence decreases
                           :microtheory kb-current-mt)))
              (push new-rel new-relations))))))
    
    ;; Causal chaining
    (dolist (rel1 kb-event-relations)
      (when (eq (kb-event-relation-type rel1) :causes)
        (dolist (rel2 kb-event-relations)
          (when (and (eq (kb-event-relation-type rel2) :causes)
                    (eq (kb-event-relation-target-event rel1)
                        (kb-event-relation-source-event rel2)))
            ;; A causes B, B causes C => A causes C (indirect)
            (let ((new-rel (kb-event-relation-create
                           :type :indirectly-causes
                           :source-event (kb-event-relation-source-event rel1)
                           :target-event (kb-event-relation-target-event rel2)
                           :strength (* (kb-event-relation-strength rel1)
                                      (kb-event-relation-strength rel2)
                                      0.7)
                           :microtheory kb-current-mt)))
              (push new-rel new-relations))))))
    
    ;; Add new relations
    (dolist (rel new-relations)
      (push rel kb-event-relations)
      (kb-add-fact (kb-event-relation-source-event rel)
                   (kb-event-relation-type rel)
                   (kb-event-relation-target-event rel)))
    
    new-relations))

(defun kb-find-events-by-type (event-type &optional mt-name)
  "Find all events of a given type."
  (let ((results nil)
        (mt-name (or mt-name kb-current-mt)))
    (maphash 
     (lambda (id event)
       (when (and (eq (kb-event-type event) event-type)
                 (eq (kb-event-microtheory event) mt-name))
         (push id results)))
     kb-events)
    results))

(defun kb-find-events-by-participant (participant &optional role mt-name)
  "Find events involving a specific participant, optionally in a specific role."
  (let ((results nil)
        (mt-name (or mt-name kb-current-mt)))
    (maphash 
     (lambda (id event)
       (when (and (member participant (kb-event-participants event))
                 (eq (kb-event-microtheory event) mt-name)
                 (or (null role)
                     (plist-get (kb-event-roles event) role)))
         (push id results)))
     kb-events)
    results))

(defun kb-find-events-at-time (time &optional mt-name)
  "Find events that occur at or contain a specific time."
  (let ((results nil)
        (mt-name (or mt-name kb-current-mt)))
    (maphash 
     (lambda (id event)
       (when (and (kb-event-contains-time-p event time)
                 (eq (kb-event-microtheory event) mt-name))
         (push id results)))
     kb-events)
    results))

(defun kb-event-contains-time-p (event time)
  "Check if an event contains or occurs at a specific time."
  (let ((start (kb-event-start-time event))
        (end (kb-event-end-time event)))
    (cond
     ((and start end)
      (and (time-less-p start time) (time-less-p time end)))
     (start
      (time-equal-p start time))
     (t nil))))

;;; Event Planning and Generation

(defun kb-generate-event-instance (process-type participants &rest properties)
  "Generate an event instance from a process type."
  (let* ((process (kb-get-process process-type))
         (event-props (append properties
                             (list :participants participants))))
    
    (when process
      ;; Add typical properties from process
      (when (kb-process-typical-duration process)
        (unless (plist-get properties :duration)
          (setq event-props (plist-put event-props :duration 
                                      (kb-process-typical-duration process)))))
      
      ;; Create the event
      (let ((event-id (apply #'kb-create-event process-type event-props)))
        
        ;; Apply preconditions and effects
        (kb-apply-process-preconditions process event-id)
        (kb-apply-process-effects process event-id)
        
        event-id))))

(defun kb-apply-process-preconditions (process event-id)
  "Apply process preconditions to an event."
  (dolist (precond (kb-process-preconditions process))
    ;; Add precondition facts
    (kb-add-fact event-id 'requires precond)))

(defun kb-apply-process-effects (process event-id)
  "Apply process effects to an event."
  (dolist (effect (kb-process-effects process))
    ;; Add effect facts
    (kb-add-fact event-id 'causes effect)))

;;; Event Queries

(defun kb-query-events (pattern &optional mt-name)
  "Query events using a pattern."
  (let ((results nil)
        (mt-name (or mt-name kb-current-mt)))
    
    ;; Pattern matching on events
    (maphash 
     (lambda (id event)
       (when (and (kb-event-matches-pattern-p event pattern)
                 (eq (kb-event-microtheory event) mt-name))
         (push id results)))
     kb-events)
    
    results))

(defun kb-event-matches-pattern-p (event pattern)
  "Check if an event matches a query pattern."
  (cl-every 
   (lambda (constraint)
     (let ((property (car constraint))
           (value (cadr constraint)))
       (pcase property
         (:type (eq (kb-event-type event) value))
         (:participant (member value (kb-event-participants event)))
         (:location (equal (kb-event-location event) value))
         (:before (and (kb-event-start-time event)
                      (time-less-p (kb-event-start-time event) value)))
         (:after (and (kb-event-end-time event)
                     (time-less-p value (kb-event-end-time event))))
         (_ t))))
   pattern))

;;; Example Setup

(defun kb-setup-event-examples ()
  "Set up example events and processes."
  
  ;; Define some process types
  (kb-define-process 'walking
                     :typical-duration 300  ; 5 minutes
                     :typical-participants '(agent)
                     :preconditions '((agent can-walk t))
                     :effects '((agent location changed)))
  
  (kb-define-process 'meeting
                     :typical-duration 3600  ; 1 hour
                     :typical-participants '(organizer attendees)
                     :preconditions '((location available t))
                     :effects '((attendees informed t)))
  
  ;; Create some events
  (let ((walk1 (kb-create-event 'walking
                               :participants '(John)
                               :start-time (current-time)
                               :location 'park)))
    
    (let ((walk2 (kb-create-event 'walking
                                 :participants '(Mary)
                                 :start-time (time-add (current-time) 300)
                                 :location 'park)))
      
      ;; Add temporal relation
      (kb-add-event-relation :before walk1 walk2))))

(provide 'kb-events)
;;; kb-events.el ends here
