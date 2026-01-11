;;; test_generated_facts.el --- Generated Knowledge Base Facts
;; -*- lexical-binding: t; -*-

;; Generated on: 2025-09-03T03:14:04.931891
;;; Simple pattern-based fact extraction

(require 'kb-advanced-system)

;; Initialize KB system if not already done
(unless (boundp 'kb-current-mt)
  (kb-init))

 ;; Create and switch to microtheory ScientistMt
 (unless (kb-get-microtheory 'ScientistMt)
   (kb-create-microtheory 'ScientistMt '(CommonSenseMt)))

(in-microtheory ScientistMt
  ;; Extracted facts (6 total)
  (kb-assert 'Albert_Einstein 'is-a 'theoretical_physicist_born_in_Ulm 0.9 nil 'ScientistMt)
  (kb-assert 'Marie_Curie 'is-a 'physicist_and_chemist_who_conducted_pioneering_research_on_radioactivity 0.9 nil 'ScientistMt)
  (kb-assert 'Albert_Einstein_was_a_theoretical_physicist 'born-in 'Ulm 0.85 nil 'ScientistMt)
  (kb-assert 'She_was 'born-in 'Warsaw_in_1867_and_later_moved_to_Paris_to_study_at_the_University_of_Paris 0.85 nil 'ScientistMt)
  (kb-assert 'Einstein 'worked-at 'Princeton_University 0.85 nil 'ScientistMt)
  (kb-assert 'He 'developed 'theory_of_relativity 0.8 nil 'ScientistMt)

)

;; Perform reasoning to infer additional facts
(in-microtheory ScientistMt
  (kb-reason))

;; Display status
(kb-status)
