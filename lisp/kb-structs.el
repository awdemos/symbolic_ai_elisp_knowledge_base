;;; kb-structs.el --- Core data structures for Knowledge Base
;; -*- lexical-binding: t; -*-

;; Author: AI Assistant
;; Keywords: ai, knowledge base, structures
;; Version: 1.0

;;; Commentary:

;; This package defines the core data structures used throughout the
;; Knowledge Base system. Centralizing these structures prevents
;; circular dependencies and ensures consistent definitions.

;;; Code:

(require 'cl-lib)

;;; Microtheory Structure

(cl-defstruct (kb-microtheory (:constructor kb-microtheory-create)
                              (:copier nil))
  "A microtheory context for scoped knowledge."
  name
  parent-mts        ; list of direct parent microtheories
  facts            ; hash table of facts in this mt
  rules            ; list of inference rules in this mt
  inherits-from    ; list of mts to inherit from (deprecated, use parent-mts)
  temp-p           ; t if this is a temporary microtheory
  created-at       ; timestamp when created
  priority         ; priority for conflict resolution (higher = more specific)
  inheritance-mode ; 'strict, 'override, 'merge
  local-facts)     ; facts that should not be inherited

;;; Fact Structure

(cl-defstruct (kb-fact (:constructor kb-fact-create)
                       (:copier nil))
  "A structure representing a fact in the knowledge base."
  subject predicate object
  certainty
  microtheory      ; which microtheory this fact belongs to
  justification    ; how this fact was derived
  temporal-info)   ; temporal validity information

;;; Rule Structure

(cl-defstruct (kb-rule (:constructor kb-rule-create)
                       (:copier nil))
  "A structure representing an inference rule."
  name premises conclusion
  microtheory      ; which microtheory this rule belongs to
  priority         ; rule priority for conflict resolution
  temporal-p)      ; whether this rule handles temporal reasoning

;;; Temporal Information Structure

(cl-defstruct (kb-temporal-info (:constructor kb-temporal-info-create)
                                (:copier nil))
  "Temporal information for facts and events."
  valid-from valid-to
  during-event
  happens-at)

;;; Event Structures

(cl-defstruct (kb-event (:constructor kb-event-create)
                        (:copier nil))
  "An event in the knowledge base."
  id type participants location
  start-time end-time duration
  microtheory properties temporal-info
  certainty justification created-at)

(cl-defstruct (kb-process (:constructor kb-process-create)
                          (:copier nil))
  "A process type definition."
  name typical-duration typical-participants
  preconditions effects microtheory properties)

(cl-defstruct (kb-event-relation (:constructor kb-event-relation-create)
                                 (:copier nil))
  "A relation between two events."
  relation-type from-event to-event
  strength microtheory justification)

;;; Default Rule Structures

(cl-defstruct (kb-default-rule (:constructor kb-default-rule-create)
                               (:copier nil))
  "A default rule with exceptions."
  name premises conclusion exceptions
  strength specificity microtheory
  defeated-p applies-to)

(cl-defstruct (kb-exception (:constructor kb-exception-create)
                            (:copier nil))
  "An exception to a default rule."
  name applies-to conditions conclusion
  priority microtheory)

;;; TMS Structures

(cl-defstruct kb-justification
  "A justification record for a fact."
  fact premises rule support-type
  timestamp active-p)

(cl-defstruct kb-fact-record
  "Extended fact record with TMS information."
  subject predicate object microtheory
  justifications dependents belief-status)

(provide 'kb-structs)
;;; kb-structs.el ends here
