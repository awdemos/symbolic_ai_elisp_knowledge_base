;;; programming-languages-kb.el --- Programming Languages Knowledge Base

;;; Commentary:
;; This file contains facts about various programming languages including
;; when they were created, who created them, what paradigms they support,
;; and what they're commonly used for.

;;; Code:

;; Knowledge base structure: (language creator year paradigms uses)
(defvar programming-languages-facts
  '((python "Guido van Rossum" 1991 (procedural object-oriented functional) 
            (web-development data-science machine-learning scripting automation))
    
    (javascript "Brendan Eich" 1995 (procedural object-oriented functional event-driven)
                (web-development frontend-development backend-development mobile-apps))
    
    (rust "Graydon Hoare" 2010 (procedural object-oriented functional concurrent)
          (systems-programming web-assembly game-development blockchain))
    
    (java "James Gosling" 1995 (object-oriented procedural)
          (enterprise-applications web-development mobile-apps desktop-apps))
    
    (c "Dennis Ritchie" 1972 (procedural)
       (systems-programming operating-systems embedded-systems compilers))
    
    (cpp "Bjarne Stroustrup" 1985 (procedural object-oriented generic)
         (systems-programming game-development desktop-applications embedded-systems))
    
    (go "Robert Griesemer, Rob Pike, Ken Thompson" 2009 (procedural concurrent)
        (web-services microservices cloud-infrastructure devops-tools))
    
    (lisp "John McCarthy" 1958 (functional symbolic)
          (artificial-intelligence research education symbolic-computation))
    
    (haskell "Committee" 1990 (functional lazy pure)
             (research academic-programming compiler-construction financial-modeling))
    
    (clojure "Rich Hickey" 2007 (functional concurrent immutable)
             (web-development data-processing concurrent-programming jvm-applications))
    
    (ruby "Yukihiro Matsumoto" 1995 (object-oriented procedural functional)
          (web-development scripting automation metaprogramming))
    
    (php "Rasmus Lerdorf" 1995 (procedural object-oriented)
         (web-development server-side-scripting content-management-systems))
    
    (swift "Chris Lattner" 2014 (object-oriented functional protocol-oriented)
           (ios-development macos-development mobile-apps desktop-apps))
    
    (kotlin "JetBrains" 2011 (object-oriented functional)
            (android-development jvm-applications web-development multiplatform))
    
    (typescript "Microsoft" 2012 (object-oriented functional procedural)
                (web-development frontend-frameworks large-scale-applications))
    
    (scala "Martin Odersky" 2003 (object-oriented functional)
           (jvm-applications big-data-processing web-services functional-programming))
    
    (erlang "Joe Armstrong, Robert Virding, Mike Williams" 1986 (functional concurrent actor-model)
            (telecommunications distributed-systems fault-tolerant-systems real-time-systems))
    
    (elixir "José Valim" 2011 (functional concurrent actor-model)
            (web-applications distributed-systems real-time-systems iot-applications))
    
    (r "Ross Ihaka, Robert Gentleman" 1993 (functional statistical)
       (data-analysis statistics bioinformatics research))
    
    (matlab "Cleve Moler" 1984 (procedural object-oriented)
            (numerical-computing engineering-simulation scientific-research data-analysis)))
  "Knowledge base of programming language facts.")

;; Query functions
(defun pl-get-creator (language)
  "Get the creator(s) of LANGUAGE."
  (let ((entry (assoc language programming-languages-facts)))
    (when entry
      (nth 1 entry))))

(defun pl-get-year (language)
  "Get the creation year of LANGUAGE."
  (let ((entry (assoc language programming-languages-facts)))
    (when entry
      (nth 2 entry))))

(defun pl-get-paradigms (language)
  "Get the programming paradigms supported by LANGUAGE."
  (let ((entry (assoc language programming-languages-facts)))
    (when entry
      (nth 3 entry))))

(defun pl-get-uses (language)
  "Get the common uses of LANGUAGE."
  (let ((entry (assoc language programming-languages-facts)))
    (when entry
      (nth 4 entry))))

(defun pl-get-all-info (language)
  "Get all information about LANGUAGE."
  (let ((entry (assoc language programming-languages-facts)))
    (when entry
      (list :language (nth 0 entry)
            :creator (nth 1 entry)
            :year (nth 2 entry)
            :paradigms (nth 3 entry)
            :uses (nth 4 entry)))))

(defun pl-languages-by-paradigm (paradigm)
  "Get all languages that support PARADIGM."
  (let (result)
    (dolist (entry programming-languages-facts)
      (when (member paradigm (nth 3 entry))
        (push (nth 0 entry) result)))
    (reverse result)))

(defun pl-languages-by-use (use)
  "Get all languages commonly used for USE."
  (let (result)
    (dolist (entry programming-languages-facts)
      (when (member use (nth 4 entry))
        (push (nth 0 entry) result)))
    (reverse result)))

(defun pl-languages-by-decade (decade)
  "Get all languages created in DECADE (e.g., 1990 for 1990-1999)."
  (let (result)
    (dolist (entry programming-languages-facts)
      (let ((year (nth 2 entry)))
        (when (and (>= year decade) (< year (+ decade 10)))
          (push (nth 0 entry) result))))
    (reverse result)))

(defun pl-oldest-languages (n)
  "Get the N oldest programming languages."
  (let ((sorted-langs (sort (copy-sequence programming-languages-facts)
                           (lambda (a b) (< (nth 2 a) (nth 2 b))))))
    (mapcar #'car (seq-take sorted-langs n))))

(defun pl-newest-languages (n)
  "Get the N newest programming languages."
  (let ((sorted-langs (sort (copy-sequence programming-languages-facts)
                           (lambda (a b) (> (nth 2 a) (nth 2 b))))))
    (mapcar #'car (seq-take sorted-langs n))))

;; Interactive query function
(defun pl-query-language (language)
  "Interactively query information about LANGUAGE."
  (interactive "SEnter programming language: ")
  (let ((info (pl-get-all-info language)))
    (if info
        (message "Language: %s\nCreator: %s\nYear: %d\nParadigms: %s\nUses: %s"
                 (plist-get info :language)
                 (plist-get info :creator)
                 (plist-get info :year)
                 (mapconcat #'symbol-name (plist-get info :paradigms) ", ")
                 (mapconcat #'symbol-name (plist-get info :uses) ", "))
      (message "No information found for language: %s" language))))

;; Example usage functions
(defun pl-demo ()
  "Demonstrate the programming languages knowledge base."
  (interactive)
  (with-output-to-temp-buffer "*Programming Languages KB Demo*"
    (princ "=== Programming Languages Knowledge Base Demo ===\n\n")
    
    (princ "Python information:\n")
    (let ((info (pl-get-all-info 'python)))
      (princ (format "  Creator: %s\n" (plist-get info :creator)))
      (princ (format "  Year: %d\n" (plist-get info :year)))
      (princ (format "  Paradigms: %s\n" (mapconcat #'symbol-name (plist-get info :paradigms) ", ")))
      (princ (format "  Uses: %s\n\n" (mapconcat #'symbol-name (plist-get info :uses) ", "))))
    
    (princ "Functional programming languages:\n")
    (let ((langs (pl-languages-by-paradigm 'functional)))
      (princ (format "  %s\n\n" (mapconcat #'symbol-name langs ", "))))
    
    (princ "Languages for web development:\n")
    (let ((langs (pl-languages-by-use 'web-development)))
      (princ (format "  %s\n\n" (mapconcat #'symbol-name langs ", "))))
    
    (princ "Languages created in the 1990s:\n")
    (let ((langs (pl-languages-by-decade 1990)))
      (princ (format "  %s\n\n" (mapconcat #'symbol-name langs ", "))))
    
    (princ "5 oldest languages:\n")
    (let ((langs (pl-oldest-languages 5)))
      (princ (format "  %s\n\n" (mapconcat #'symbol-name langs ", "))))
    
    (princ "5 newest languages:\n")
    (let ((langs (pl-newest-languages 5)))
      (princ (format "  %s\n" (mapconcat #'symbol-name langs ", "))))))

(provide 'programming-languages-kb)

;;; programming-languages-kb.el ends here