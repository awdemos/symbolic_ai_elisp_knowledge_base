;;; test-temporal.el --- Temporal reasoning tests

;;; Commentary:
;; Tests for time-bounded facts and temporal queries.

;;; Code:

(require 'ert)
(require 'kb-advanced-system)

(ert-deftest test-temporal-assertion ()
  "Test basic temporal fact assertion."
  (kb-init)
  
  (kb-assert-temporal 'John 'location 'office "2025-01-01" "2025-01-02")
  
  ;; Should be able to query temporal facts
  (let ((result (kb-query-temporal 'John 'location "2025-01-01")))
    (should result)
    (should (member 'office result))))

(ert-deftest test-temporal-bounds ()
  "Test that temporal facts respect time bounds."
  (kb-init)
  
  (kb-assert-temporal 'John 'location 'office "2025-01-01" "2025-01-02")
  
  ;; Should be true during the interval
  (should (kb-query-temporal 'John 'location "2025-01-01"))
  
  ;; Should be false outside the interval
  (should-not (kb-query-temporal 'John 'location "2024-12-31"))
  (should-not (kb-query-temporal 'John 'location "2025-01-03")))

(ert-deftest test-overlapping-temporal-facts ()
  "Test handling of overlapping temporal intervals."
  (kb-init)
  
  (kb-assert-temporal 'John 'location 'office "2025-01-01" "2025-01-05")
  (kb-assert-temporal 'John 'location 'home "2025-01-03" "2025-01-07")
  
  ;; Before overlap: only office
  (let ((result (kb-query-temporal 'John 'location "2025-01-02")))
    (should (member 'office result))
    (should-not (member 'home result)))
  
  ;; During overlap: both locations possible
  (let ((result (kb-query-temporal 'John 'location "2025-01-04")))
    (should (or (member 'office result) (member 'home result)))))

(ert-deftest test-event-creation ()
  "Test event creation with temporal properties."
  (kb-init)
  
  (kb-create-event 'meeting-1 :participants '(Alice Bob) :start "2025-01-01T09:00" :end "2025-01-01T10:00")
  
  ;; Event should exist
  (should (kb-query 'meeting-1 'is-a))
  
  ;; Participants should be recorded
  (let ((participants (kb-query 'meeting-1 'participants)))
    (should (member 'Alice participants))
    (should (member 'Bob participants))))

;;; test-temporal.el ends here
