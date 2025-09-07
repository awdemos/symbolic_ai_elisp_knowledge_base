;;; test-simple-persistence.el --- Simple tests for KB Persistence System

;; Author: AI Assistant
;; Keywords: ai, knowledge base, persistence, testing
;; Version: 1.0

;;; Commentary:

;; Simple test suite for the KB persistence system

;;; Code:

(require 'ert)
(require 'kb-advanced-system)

(defun test-clean-kb ()
  "Clean up the KB state."
  (setq kb-microtheories (make-hash-table :test 'equal))
  (setq kb-events (make-hash-table :test 'equal))
  (setq kb-processes (make-hash-table :test 'equal))
  (setq kb-default-rules nil)
  (setq kb-exceptions nil)
  (setq kb-current-mt 'BaseMt)
  (when (boundp 'kb-tms-facts)
    (setq kb-tms-facts (make-hash-table :test 'equal)))
  (when (boundp 'kb-tms-justifications)
    (setq kb-tms-justifications (make-hash-table :test 'equal))))

(ert-deftest test-basic-save-only ()
  "Test basic save functionality without loading."
  (test-clean-kb)
  (kb-init)
  
  ;; Add some basic facts
  (kb-assert 'Socrates 'is-a 'human 0.9)
  (kb-assert 'human 'is-a 'mammal 1.0)
  
  (let ((test-file "/tmp/test-kb-basic.el"))
    ;; Save the KB
    (kb-save test-file)
    (should (file-exists-p test-file))
    
    ;; Check file has content
    (should (> (file-attribute-size (file-attributes test-file)) 100))
    
    ;; Clean up
    (delete-file test-file)))

(ert-deftest test-persistence-status ()
  "Test persistence status function."
  (test-clean-kb)
  (kb-init)
  (should (functionp #'kb-persistence-status))
  (kb-persistence-status))  ; Should not error

(ert-deftest test-backup-creation ()
  "Test backup file creation."
  (test-clean-kb)
  (kb-init)
  
  ;; Add some data
  (kb-assert 'test 'prop 'value)
  
  (let ((original-dir default-directory))
    (unwind-protect
        (progn
          (cd "/tmp")
          (kb-backup)
          ;; Check that a backup file was created
          (let ((backup-files (directory-files "." nil "kb-backup-.*\\.el$")))
            (should backup-files)
            ;; Clean up backup files
            (dolist (file backup-files)
              (delete-file file))))
      (cd original-dir))))

(provide 'test-simple-persistence)
;;; test-simple-persistence.el ends here
