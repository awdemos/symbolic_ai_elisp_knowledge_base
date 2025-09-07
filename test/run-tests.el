;;; run-tests.el --- Test runner for KB system

;;; Commentary:
;; Test runner that loads all test files and runs them.

;;; Code:

(require 'ert)

;; Add lisp directory to load path
(add-to-list 'load-path (expand-file-name "../lisp" (file-name-directory load-file-name)))

;; Load the main system
(require 'kb-advanced-system)

;; Load all test files
(load-file (expand-file-name "test-core.el" (file-name-directory load-file-name)))
(load-file (expand-file-name "test-microtheories.el" (file-name-directory load-file-name)))
(load-file (expand-file-name "test-nonmonotonic.el" (file-name-directory load-file-name)))
(load-file (expand-file-name "test-temporal.el" (file-name-directory load-file-name)))

;;; run-tests.el ends here
