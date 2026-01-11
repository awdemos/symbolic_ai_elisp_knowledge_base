;;; kb-events.el --- Event Reification and Event-based Reasoning
;; -*- lexical-binding: t; -*-

;; Author: AI Assistant
;; Keywords: ai, events, reification, processes
;; Version: 2.0

;;; Commentary:

;; This package implements event reification for advanced reasoning,
;; treating events and processes as first-class objects that can be
;; reasoned about, related, and queried.
;;
;; Features:
;; - Event creation and management
;; - Process types and definitions
;; - Event-relation support (before, after, concurrent, causal)
;; - Event querying by participants, time, and relations
;; - Integration with microtheory contexts
;; - Temporal reasoning over event sequences
;;
;; Event Relations:
;; - Event-before: E1 occurs before E2
;; - Event-after: E1 occurs after E2
;; - Event-during: E1 overlaps temporally with E2
;; - Event-causal: E1 causes E2
;; - Event-concurrent: E1 and E2 happen at same time

;;; Code:

(require 'cl-lib)
(require 'kb-microtheories)
(require 'kb-tms)

;;; Variables

(defconst kb-event-counter-start 1000
  "Starting counter for generating event IDs.")

(defvar kb-event-counter kb-event-counter-start
  "Counter for generating unique event IDs.")

(defvar kb-events (make-hash-table :test 'equal)
  "Hash table storing all events in the system.")

(defvar kb-processes (make-hash-table :test 'equal)
  "Hash table storing process type definitions.")

(defvar kb-event-relations (make-hash-table :test 'equal)
  "Hash table storing event relations.")

(defvar kb-current-event nil
  "Currently active event (if any).")

;;; Structures

(cl-defstruct (kb-event (:constructor kb-event-create)
                                (:copier nil))
  "An event in the knowledge base."
  id                ; unique event identifier
  type              ; event type (e.g., 'meeting, 'walking)
  participants      ; list of entities participating
  location          ; where event occurs
  start-time        ; when event starts
  end-time          ; when event ends (nil if ongoing)
  duration          ; how long event lasts (in seconds)
  microtheory       ; microtheory containing event
  properties        ; additional properties as plist
  temporal-info     ; temporal bounds for event
  certainty         ; confidence level (0.0 to 1.0)
  justification     ; TMS justification for event
  created-at        ; timestamp when event was created)

(cl-defstruct (kb-process (:constructor kb-process-create)
                                 (:copier nil))
  "A process type definition."
  name              ; process name/identifier
  typical-duration ; expected duration in seconds
  typical-participants ; list of participant types
  preconditions     ; list of required conditions
  effects          ; list of effects after process
  microtheory      ; microtheory where process is defined
  properties        ; additional properties as plist)

(cl-defstruct (kb-event-relation (:constructor kb-event-relation-create)
                                     (:copier nil))
  "A relation between two events."
  relation-type    ; :event-before, :event-after, :event-during, :event-causal, :event-concurrent
  from-event       ; event ID that relation originates from
  to-event         ; event ID that relation points to
  strength         ; confidence in the relation (0.0 to 1.0)
  microtheory      ; microtheory where relation holds
  justification     ; TMS justification for the relation
  created-at       ; timestamp when relation was created)

;;; Event Management API

;;;###autoload
(defun kb-create-event (event-type &rest properties)
  "Create a new event.
EVENT-TYPE is the type of event to create.
PROPERTIES are keyword-value pairs for event properties.
Returns the created event object."
  (interactive "sCreate event (s): ")
  (kb-with-validation kb-create-event (list event-type properties)
    (kb-with-error-recovery
      (let* ((kb-current-mt (or (plist-get properties :mt) kb-current-mt))
             (mt-obj (kb-get-microtheory kb-current-mt)))
        (unless mt-obj
          (signal 'kb-microtheory-error (list "Microtheory not found" kb-current-mt)))
        
        ;; Generate unique event ID
        (cl-incf kb-event-counter)
        (let* ((id (format "Event-%d" kb-event-counter))
               (start-time (or (plist-get properties :start-time) (current-time)))
               (end-time (plist-get properties :end-time))
               (duration (or (plist-get properties :duration)
                             (when (and end-time start-time)
                               (float-time (time-subtract end-time start-time)))))
               (participants (or (plist-get properties :participants) '()))
               (location (plist-get properties :location))
               (certainty (or (plist-get properties :certainty) 1.0))
               (temporal-info (list :valid-from start-time :valid-to end-time)))
          
          ;; Create event structure
          (let ((event (kb-event-create
                       :id id
                       :type event-type
                       :participants participants
                       :location location
                       :start-time start-time
                       :end-time end-time
                       :duration duration
                       :microtheory kb-current-mt
                       :properties properties
                       :temporal-info temporal-info
                       :certainty certainty
                       :justification nil
                       :created-at (current-time))))
            
            ;; Store event
            (puthash id event kb-events)
            
            ;; Add event to microtheory facts
            (kb-add-fact-with-justification
             (intern id)
             'type
             event-type
             `((kb-event-id ,id))
             'event-created
             (list id start-time end-time))
             certainty
             kb-current-mt)
            
            ;; Return event
            event))))))

;;;###autoload
(defun kb-get-event (event-id)
  "Retrieve an event by its ID.
EVENT-ID is the unique identifier of the event."
  (interactive "sGet event by ID: ")
  (gethash event-id kb-events))

;;;###autoload
(defun kb-find-events-by-participant (participant &optional mt-name)
  "Find all events where PARTICIPANT is involved.
MT-NAME limits search to a specific microtheory."
  (let ((results nil))
    (maphash (lambda (key event)
               (when (member participant (kb-event-participants event))
                 (push event results)))
             kb-events)
    (if mt-name
        (cl-remove-if-not (lambda (event) (eq (kb-event-microtheory event) mt-name)) results)
      results)))

;;;###autoload
(defun kb-find-events-by-time-range (start-time end-time &optional mt-name)
  "Find all events within a time range.
START-TIME and END-TIME define the time window.
MT-NAME limits search to a specific microtheory."
  (let ((results nil))
    (maphash (lambda (key event)
               (when (and (kb-event-start-time event)
                         (time-less-p start-time (kb-event-start-time event))
                         (time-less-p (kb-event-start-time event) end-time))
                 (push event results)))
             kb-events)
    (if mt-name
        (cl-remove-if-not (lambda (event) (eq (kb-event-microtheory event) mt-name)) results)
      results)))

;;;###autoload
(defun kb-find-events-by-type (event-type &optional mt-name)
  "Find all events of a specific type.
EVENT-TYPE is the type of events to find.
MT-NAME limits search to a specific microtheory."
  (let ((results nil))
    (maphash (lambda (key event)
               (when (eq (kb-event-type event) event-type)
                 (push event results)))
             kb-events)
    (if mt-name
        (cl-remove-if-not (lambda (event) (eq (kb-event-microtheory event) mt-name)) results)
      results)))

;;; Event Relations API

;;;###autoload
(defun kb-add-event-relation (relation-type from-event to-event &optional strength mt)
  "Add a relation between two events.
RELATION-TYPE is one of: :event-before, :event-after, :event-during, :event-causal, :event-concurrent.
FROM-EVENT and TO-EVENT are event IDs.
STRENGTH is confidence in the relation (default 1.0).
MT is the microtheory where the relation holds."
  (interactive "sAdd relation (s): ")
  (kb-with-validation kb-add-event-relation (list relation-type from-event to-event strength mt)
    (kb-with-error-recovery
      (let* ((kb-current-mt (or mt kb-current-mt))
             (from-event-obj (kb-get-event from-event))
             (to-event-obj (kb-get-event to-event)))
        (unless (and from-event-obj to-event-obj)
          (signal 'kb-validation-error '("Both events must exist")))
        
        (let ((relation (kb-event-relation-create
                        :relation-type relation-type
                        :from-event from-event
                        :to-event to-event
                        :strength (or strength 1.0)
                        :microtheory kb-current-mt
                        :justification nil
                        :created-at (current-time))))
          
          ;; Store relation
          (let ((relation-key (list from-event to-event relation-type)))
            (puthash relation-key relation kb-event-relations)
            
            ;; Add as microtheory fact
            (kb-add-fact-with-justification
             (intern from-event)
             'event-relation
             relation-type
             (list to-event from-event)
             strength
             kb-current-mt)
            
            relation))))))

;;;###autoload
(defun kb-get-event-relations (event-id)
  "Get all relations involving an event.
EVENT-ID is the event ID."
  (let ((relations nil))
    (maphash (lambda (key relation)
               (when (or (eq (kb-event-relation-from-event relation) event-id)
                         (eq (kb-event-relation-to-event relation) event-id))
                 (push relation relations)))
             kb-event-relations)
    relations))

;;; Process Management API

;;;###autoload
(defun kb-define-process (name &key typical-duration typical-participants preconditions effects &rest properties)
  "Define a process type.
NAME is the identifier for the process.
TYPICAL-DURATION is the expected duration in seconds.
TYPICAL-PARTICIPANTS is a list of participant types.
PRECONDITIONS is a list of conditions that must hold.
EFFECTS is a list of conditions that result after process.
PROPERTIES are additional keyword-value pairs."
  (interactive "sDefine process (s): ")
  (let ((process (kb-process-create
                  :name name
                  :typical-duration typical-duration
                  :typical-participants typical-participants
                  :preconditions preconditions
                  :effects effects
                  :microtheory kb-current-mt
                  :properties properties)))
    (puthash name process kb-processes)
    process))

;;;###autoload
(defun kb-get-process (process-name)
  "Retrieve a process definition by name.
PROCESS-NAME is the identifier of the process."
  (gethash process-name kb-processes))

;;; Query and Reasoning API

;;;###autoload
(defun kb-event-before-p (event-id-1 event-id-2)
  "Check if EVENT-ID-1 occurs before EVENT-ID-2."
  (let ((relations (kb-get-event-relations event-id-1))
        (before-p nil))
    (dolist (relation relations)
      (when (eq (kb-event-relation-relation-type relation) :event-before)
        (setq before-p t)))
    before-p))

;;;###autoload
(defun kb-event-after-p (event-id-1 event-id-2)
  "Check if EVENT-ID-1 occurs after EVENT-ID-2."
  (let ((relations (kb-get-event-relations event-id-1))
        (after-p nil))
    (dolist (relation relations)
      (when (eq (kb-event-relation-relation-type relation) :event-after)
        (setq after-p t)))
    after-p))

;;;###autoload
(defun kb-events-overlap-p (event-id-1 event-id-2)
  "Check if EVENT-ID-1 overlaps temporally with EVENT-ID-2."
  (let ((event-1 (kb-get-event event-id-1))
        (event-2 (kb-get-event event-id-2)))
    (and (kb-event-start-time event-1) (kb-event-start-time event-2)
         (kb-event-end-time event-1) (kb-event-end-time event-2)
         (or (time-less-p (kb-event-end-time event-1) (kb-event-start-time event-2))
             (time-less-p (kb-event-end-time event-2) (kb-event-start-time event-1))))))

;;; Event Status

;;;###autoload
(defun kb-event-ongoing-p (event-id)
  "Check if an event is currently ongoing (has no end time)."
  (let ((event (kb-get-event event-id)))
    (and event (null (kb-event-end-time event)))))

;;;###autoload
(defun kb-event-completed-p (event-id)
  "Check if an event is completed (has end time and end time is past)."
  (let ((event (kb-get-event event-id)))
    (and event (kb-event-end-time event)
         (time-less-p (kb-event-end-time event) (current-time)))))

;;; Utility Functions

(defun kb-event-signature (event)
  "Generate a unique signature for an event."
  (list (kb-event-id event) (kb-event-type event)))

(defun kb-event-status-text (event)
  "Get a human-readable status text for an event."
  (cond
   ((not (kb-get-event event))
    "Event not found")
   ((kb-event-ongoing-p (kb-event-id event))
    "Ongoing")
   ((kb-event-completed-p (kb-event-id event))
    "Completed")
   (t "Unknown")))

(provide 'kb-events)
;;; kb-events.el ends here
