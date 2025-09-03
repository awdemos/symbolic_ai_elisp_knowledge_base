;;; kb-cache.el --- Query Caching System for Knowledge Base

;; Author: AI Assistant
;; Keywords: ai, caching, performance, optimization
;; Version: 2.1

;;; Commentary:

;; This package provides optional query caching to improve performance
;; of frequently accessed knowledge base queries and inferences.

;;; Code:

(require 'cl-lib)
(require 'kb-microtheories)

;;; Cache Structures

(cl-defstruct (kb-cache-entry (:constructor kb-cache-entry-create)
                              (:copier nil))
  "A cached query result."
  key                    ; unique key for the query
  result                 ; cached result
  timestamp              ; when it was cached
  access-count           ; how many times accessed
  last-access            ; last access time
  microtheory           ; which microtheory it's from
  ttl)                   ; time-to-live in seconds

(cl-defstruct (kb-cache-stats (:constructor kb-cache-stats-create)
                              (:copier nil))
  "Cache statistics."
  hits
  misses
  evictions
  total-queries
  total-size
  start-time)

;;; Variables

(defvar kb-cache-enabled nil
  "Whether query caching is enabled.")

(defvar kb-cache-table (make-hash-table :test 'equal)
  "Main cache hash table.")

(defvar kb-cache-max-size 1000
  "Maximum number of entries in cache.")

(defvar kb-cache-default-ttl 300
  "Default time-to-live for cache entries in seconds.")

(defvar kb-cache-stats (kb-cache-stats-create
                       :hits 0 :misses 0 :evictions 0
                       :total-queries 0 :total-size 0
                       :start-time (current-time))
  "Cache statistics.")

(defvar kb-cache-invalidation-hooks nil
  "Hooks to run when cache needs invalidation.")

;;; Core Caching Functions

(defun kb-cache-enable (&optional max-size default-ttl)
  "Enable query caching with optional parameters."
  (interactive)
  (setq kb-cache-enabled t)
  (when max-size
    (setq kb-cache-max-size max-size))
  (when default-ttl
    (setq kb-cache-default-ttl default-ttl))
  (kb-cache-reset-stats)
  (message "KB caching enabled (max-size: %d, default-ttl: %ds)" 
           kb-cache-max-size kb-cache-default-ttl))

(defun kb-cache-disable ()
  "Disable query caching and clear cache."
  (interactive)
  (setq kb-cache-enabled nil)
  (kb-cache-clear)
  (message "KB caching disabled"))

(defun kb-cache-clear ()
  "Clear all cached entries."
  (interactive)
  (clrhash kb-cache-table)
  (setf (kb-cache-stats-total-size kb-cache-stats) 0)
  (message "Cache cleared"))

(defun kb-cache-key (operation &rest args)
  "Generate a cache key for an operation and arguments."
  (format "%s:%s" operation 
          (mapconcat (lambda (arg) (format "%S" arg)) args ":")))

(defun kb-cache-get (key)
  "Get an entry from the cache."
  (when kb-cache-enabled
    (let ((entry (gethash key kb-cache-table)))
      (when entry
        (if (kb-cache-entry-expired-p entry)
            ;; Entry expired, remove it
            (progn
              (remhash key kb-cache-table)
              (cl-decf (kb-cache-stats-total-size kb-cache-stats))
              nil)
          ;; Entry valid, update access info
          (setf (kb-cache-entry-last-access entry) (current-time))
          (cl-incf (kb-cache-entry-access-count entry))
          (cl-incf (kb-cache-stats-hits kb-cache-stats))
          (kb-cache-entry-result entry))))))

(defun kb-cache-put (key result &optional ttl mt)
  "Put a result in the cache."
  (when kb-cache-enabled
    (let ((ttl (or ttl kb-cache-default-ttl))
          (mt (or mt kb-current-mt)))
      
      ;; Check if cache is full
      (when (>= (hash-table-count kb-cache-table) kb-cache-max-size)
        (kb-cache-evict-lru))
      
      ;; Create and store entry
      (let ((entry (kb-cache-entry-create
                   :key key
                   :result result
                   :timestamp (current-time)
                   :access-count 1
                   :last-access (current-time)
                   :microtheory mt
                   :ttl ttl)))
        (puthash key entry kb-cache-table)
        (cl-incf (kb-cache-stats-total-size kb-cache-stats))))))

(defun kb-cache-entry-expired-p (entry)
  "Check if a cache entry has expired."
  (let ((age (float-time (time-subtract (current-time) 
                                       (kb-cache-entry-timestamp entry)))))
    (> age (kb-cache-entry-ttl entry))))

(defun kb-cache-evict-lru ()
  "Evict least recently used entries."
  (let ((entries nil))
    ;; Collect all entries with their last access time
    (maphash (lambda (key entry)
               (push (cons (kb-cache-entry-last-access entry) key) entries))
             kb-cache-table)
    
    ;; Sort by last access time (oldest first)
    (setq entries (sort entries (lambda (a b) (time-less-p (car a) (car b)))))
    
    ;; Remove oldest entries until we're under the limit
    (let ((to-remove (- (hash-table-count kb-cache-table) 
                       (floor (* kb-cache-max-size 0.8)))))  ; Remove 20%
      (dotimes (i to-remove)
        (when entries
          (remhash (cdar entries) kb-cache-table)
          (cl-incf (kb-cache-stats-evictions kb-cache-stats))
          (cl-decf (kb-cache-stats-total-size kb-cache-stats))
          (setq entries (cdr entries)))))))

;;; Cache-enabled Query Functions

(defun kb-cached-query (subject predicate &optional mt)
  "Query with caching support."
  (cl-incf (kb-cache-stats-total-queries kb-cache-stats))
  
  (if kb-cache-enabled
      (let* ((key (kb-cache-key "query" subject predicate (or mt kb-current-mt)))
             (cached-result (kb-cache-get key)))
        (if cached-result
            cached-result
          ;; Cache miss - perform query and cache result
          (cl-incf (kb-cache-stats-misses kb-cache-stats))
          (let ((result (kb-query-with-inheritance subject predicate mt)))
            (kb-cache-put key result nil mt)
            result)))
    ;; Caching disabled - direct query
    (kb-query-with-inheritance subject predicate mt)))

(defun kb-cached-inference (query &optional mt timeout)
  "Inference with caching support."
  (cl-incf (kb-cache-stats-total-queries kb-cache-stats))
  
  (if kb-cache-enabled
      (let* ((key (kb-cache-key "inference" query (or mt kb-current-mt)))
             (cached-result (kb-cache-get key)))
        (if cached-result
            cached-result
          ;; Cache miss - perform inference and cache result
          (cl-incf (kb-cache-stats-misses kb-cache-stats))
          (let ((result (kb-inference-strategist query mt timeout)))
            ;; Use longer TTL for expensive inference
            (kb-cache-put key result (* kb-cache-default-ttl 2) mt)
            result)))
    ;; Caching disabled - direct inference
    (kb-inference-strategist query mt timeout)))

;;; Cache Invalidation

(defun kb-cache-invalidate-microtheory (mt-name)
  "Invalidate all cache entries for a specific microtheory."
  (when kb-cache-enabled
    (let ((keys-to-remove nil))
      (maphash (lambda (key entry)
                 (when (eq (kb-cache-entry-microtheory entry) mt-name)
                   (push key keys-to-remove)))
               kb-cache-table)
      
      (dolist (key keys-to-remove)
        (remhash key kb-cache-table)
        (cl-decf (kb-cache-stats-total-size kb-cache-stats)))
      
      (message "Invalidated %d cache entries for microtheory %s" 
               (length keys-to-remove) mt-name))))

(defun kb-cache-invalidate-subject (subject)
  "Invalidate cache entries involving a specific subject."
  (when kb-cache-enabled
    (let ((keys-to-remove nil))
      (maphash (lambda (key entry)
                 ;; Check if key contains the subject
                 (when (string-match-p (format "%S" subject) key)
                   (push key keys-to-remove)))
               kb-cache-table)
      
      (dolist (key keys-to-remove)
        (remhash key kb-cache-table)
        (cl-decf (kb-cache-stats-total-size kb-cache-stats)))
      
      (when keys-to-remove
        (message "Invalidated %d cache entries for subject %s" 
                 (length keys-to-remove) subject)))))

(defun kb-cache-auto-invalidate-on-add (subject predicate object &optional certainty temporal-info)
  "Automatically invalidate relevant cache entries when facts are added."
  (when kb-cache-enabled
    (kb-cache-invalidate-subject subject)
    ;; Also invalidate any inference that might involve this subject
    (kb-cache-invalidate-pattern "inference.*%S" subject)))

(defun kb-cache-invalidate-pattern (pattern &rest args)
  "Invalidate cache entries matching a pattern."
  (when kb-cache-enabled
    (let ((regex (apply #'format pattern args))
          (keys-to-remove nil))
      (maphash (lambda (key entry)
                 (when (string-match-p regex key)
                   (push key keys-to-remove)))
               kb-cache-table)
      
      (dolist (key keys-to-remove)
        (remhash key kb-cache-table)
        (cl-decf (kb-cache-stats-total-size kb-cache-stats))))))

;;; Cache Statistics and Monitoring

(defun kb-cache-show-stats ()
  "Display cache statistics."
  (interactive)
  (let ((stats kb-cache-stats)
        (hit-rate (if (> (kb-cache-stats-total-queries kb-cache-stats) 0)
                     (* 100.0 (/ (float (kb-cache-stats-hits kb-cache-stats))
                                (kb-cache-stats-total-queries kb-cache-stats)))
                   0.0)))
    
    (message "Cache Stats - Size: %d/%d, Hits: %d, Misses: %d, Hit Rate: %.1f%%, Evictions: %d"
             (kb-cache-stats-total-size stats) kb-cache-max-size
             (kb-cache-stats-hits stats) (kb-cache-stats-misses stats)
             hit-rate (kb-cache-stats-evictions stats))))

(defun kb-cache-detailed-stats ()
  "Show detailed cache statistics in a buffer."
  (interactive)
  (let ((buffer-name "*KB Cache Statistics*"))
    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (insert "=== Knowledge Base Cache Statistics ===\n\n")
      
      (let ((stats kb-cache-stats)
            (hit-rate (if (> (kb-cache-stats-total-queries kb-cache-stats) 0)
                         (* 100.0 (/ (float (kb-cache-stats-hits kb-cache-stats))
                                    (kb-cache-stats-total-queries kb-cache-stats)))
                       0.0))
            (uptime (float-time (time-subtract (current-time) 
                                             (kb-cache-stats-start-time kb-cache-stats)))))
        
        (insert (format "Cache Enabled: %s\n" (if kb-cache-enabled "Yes" "No")))
        (insert (format "Current Size: %d / %d (%.1f%% full)\n" 
                       (kb-cache-stats-total-size stats) kb-cache-max-size
                       (* 100.0 (/ (float (kb-cache-stats-total-size stats)) kb-cache-max-size))))
        (insert (format "Default TTL: %d seconds\n" kb-cache-default-ttl))
        (insert (format "Uptime: %.1f minutes\n\n" (/ uptime 60)))
        
        (insert "Query Statistics:\n")
        (insert (format "  Total Queries: %d\n" (kb-cache-stats-total-queries stats)))
        (insert (format "  Cache Hits: %d\n" (kb-cache-stats-hits stats)))
        (insert (format "  Cache Misses: %d\n" (kb-cache-stats-misses stats)))
        (insert (format "  Hit Rate: %.1f%%\n" hit-rate))
        (insert (format "  Evictions: %d\n\n" (kb-cache-stats-evictions stats)))
        
        ;; Show top cached queries
        (insert "Top Cached Queries:\n")
        (let ((entries nil))
          (maphash (lambda (key entry)
                     (push (cons (kb-cache-entry-access-count entry) key) entries))
                   kb-cache-table)
          (setq entries (sort entries (lambda (a b) (> (car a) (car b)))))
          (dotimes (i (min 10 (length entries)))
            (let ((entry (nth i entries)))
              (insert (format "  %2d. %s (accessed %d times)\n" 
                             (1+ i) (cdr entry) (car entry))))))
        
        (goto-char (point-min))
        (pop-to-buffer (current-buffer))))))

(defun kb-cache-reset-stats ()
  "Reset cache statistics."
  (interactive)
  (setq kb-cache-stats (kb-cache-stats-create
                       :hits 0 :misses 0 :evictions 0
                       :total-queries 0 
                       :total-size (hash-table-count kb-cache-table)
                       :start-time (current-time)))
  (message "Cache statistics reset"))

;;; Cache Management Commands

;;;###autoload
(defun kb-cache-warm-up ()
  "Pre-populate cache with common queries."
  (interactive)
  (when kb-cache-enabled
    (message "Warming up cache...")
    (let ((start-time (current-time))
          (queries-run 0))
      
      ;; Cache common is-a queries
      (maphash (lambda (subject facts)
                 (dolist (fact facts)
                   (when (eq (kb-fact-predicate fact) 'is-a)
                     (kb-cached-query subject 'is-a (kb-fact-microtheory fact))
                     (cl-incf queries-run))))
               (kb-microtheory-facts (kb-get-microtheory 'CommonSenseMt)))
      
      (let ((duration (float-time (time-subtract (current-time) start-time))))
        (message "Cache warm-up complete: %d queries in %.2fs" queries-run duration)))))

;;;###autoload
(defun kb-cache-cleanup ()
  "Remove expired entries from cache."
  (interactive)
  (when kb-cache-enabled
    (let ((removed 0))
      (maphash (lambda (key entry)
                 (when (kb-cache-entry-expired-p entry)
                   (remhash key kb-cache-table)
                   (cl-incf removed)
                   (cl-decf (kb-cache-stats-total-size kb-cache-stats))))
               kb-cache-table)
      (message "Removed %d expired cache entries" removed))))

;;; Integration Hooks

(defun kb-cache-setup-hooks ()
  "Set up automatic cache invalidation hooks."
  ;; These would be called when facts are added/removed
  (add-hook 'kb-fact-added-hook #'kb-cache-auto-invalidate-on-add)
  (add-hook 'kb-microtheory-changed-hook #'kb-cache-invalidate-microtheory))

(defun kb-cache-remove-hooks ()
  "Remove cache invalidation hooks."
  (remove-hook 'kb-fact-added-hook #'kb-cache-auto-invalidate-on-add)
  (remove-hook 'kb-microtheory-changed-hook #'kb-cache-invalidate-microtheory))

(provide 'kb-cache)
;;; kb-cache.el ends here
