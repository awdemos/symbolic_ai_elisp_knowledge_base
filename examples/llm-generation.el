;;; llm-generation.el --- Example of LLM-generated facts integration

;;; Commentary:
;; This example shows how to integrate LLM-generated facts with the KB system.

;;; Code:

;; First, generate facts using the Python script:
;; 
;;   cd scripts/
;;   export OPENAI_API_KEY="your-key"
;;   python kb_openai_generator.py --file ../test/test_openai_sample.txt --microtheory ScientistMt
;;
;; This creates openai_facts.el which can be loaded:

(add-to-list 'load-path "./lisp")
(require 'kb-advanced-system)

;; Initialize KB
(kb-init)

;; Load generated facts (assuming they were generated)
;; (load-file "openai_facts.el")

;; Or manually add some example scientist facts
(in-microtheory ScientistMt
  (kb-assert 'Marie_Curie 'is-a 'physicist)
  (kb-assert 'Marie_Curie 'is-a 'chemist) 
  (kb-assert 'Marie_Curie 'born-in 'Warsaw)
  (kb-assert 'Marie_Curie 'worked-at 'University_of_Paris)
  (kb-assert 'Marie_Curie 'discovered 'polonium)
  (kb-assert 'Marie_Curie 'discovered 'radium)
  (kb-assert 'Marie_Curie 'won 'Nobel_Prize_Physics_1903)
  (kb-assert 'Marie_Curie 'won 'Nobel_Prize_Chemistry_1911))

;; Query the generated knowledge
(kb-query 'Marie_Curie 'is-a)
(kb-query 'Marie_Curie 'discovered)

;; Switch to the microtheory
(kb-switch-mt 'ScientistMt)

;; Perform reasoning
(kb-reason)

;; Show status
(kb-status)

;; The workflow is:
;; 1. Generate facts: python scripts/kb_openai_generator.py --file input.txt
;; 2. Load facts: (load-file "openai_facts.el") 
;; 3. Query and reason with the new knowledge

;;; llm-generation.el ends here
