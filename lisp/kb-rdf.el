;;; kb-rdf.el --- RDF/OWL Import and Export for Knowledge Base

;; Author: AI Assistant
;; Keywords: ai, rdf, owl, semantic web, import
;; Version: 2.1

;;; Commentary:

;; This package provides RDF/OWL import and export capabilities for the
;; knowledge base system, enabling interoperability with semantic web formats.

;;; Code:

(require 'cl-lib)
(require 'xml)
(require 'url)
(require 'kb-microtheories)

;;; RDF/OWL Structures

(cl-defstruct (kb-rdf-triple (:constructor kb-rdf-triple-create)
                             (:copier nil))
  "An RDF triple (subject, predicate, object)."
  subject
  predicate  
  object
  object-type                ; :uri, :literal, :blank-node
  datatype                   ; for typed literals
  language)                  ; for language-tagged literals

(cl-defstruct (kb-rdf-namespace (:constructor kb-rdf-namespace-create)
                               (:copier nil))
  "An RDF namespace declaration."
  prefix
  uri)

(cl-defstruct (kb-rdf-document (:constructor kb-rdf-document-create)
                               (:copier nil))
  "An RDF document with metadata."
  base-uri
  namespaces                 ; list of kb-rdf-namespace
  triples                    ; list of kb-rdf-triple
  format)                    ; :rdf-xml, :turtle, :n3, :nt

;;; Variables

(defvar kb-rdf-namespaces (make-hash-table :test 'equal)
  "Hash table of known RDF namespaces.")

(defvar kb-rdf-default-namespaces
  '(("rdf" . "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    ("rdfs" . "http://www.w3.org/2000/01/rdf-schema#")
    ("owl" . "http://www.w3.org/2002/07/owl#")
    ("xsd" . "http://www.w3.org/2001/XMLSchema#")
    ("foaf" . "http://xmlns.com/foaf/0.1/")
    ("dc" . "http://purl.org/dc/elements/1.1/")
    ("dcterms" . "http://purl.org/dc/terms/"))
  "Default RDF namespaces.")

(defvar kb-rdf-type-mapping
  '(("http://www.w3.org/2001/XMLSchema#string" . string)
    ("http://www.w3.org/2001/XMLSchema#integer" . integer)
    ("http://www.w3.org/2001/XMLSchema#decimal" . float)
    ("http://www.w3.org/2001/XMLSchema#boolean" . boolean)
    ("http://www.w3.org/2001/XMLSchema#dateTime" . datetime))
  "Mapping from XSD types to Elisp types.")

;;; Namespace Management

(defun kb-rdf-init-namespaces ()
  "Initialize default RDF namespaces."
  (dolist (ns kb-rdf-default-namespaces)
    (puthash (car ns) (cdr ns) kb-rdf-namespaces)))

(defun kb-rdf-add-namespace (prefix uri)
  "Add or update an RDF namespace."
  (puthash prefix uri kb-rdf-namespaces))

(defun kb-rdf-resolve-uri (qname-or-uri)
  "Resolve a qualified name or return URI as-is."
  (if (string-match "^\\([^:]+\\):\\(.+\\)$" qname-or-uri)
      (let ((prefix (match-string 1 qname-or-uri))
            (local (match-string 2 qname-or-uri)))
        (let ((namespace-uri (gethash prefix kb-rdf-namespaces)))
          (if namespace-uri
              (concat namespace-uri local)
            qname-or-uri)))  ; Return as-is if prefix not found
    qname-or-uri))  ; Already a full URI or literal

(defun kb-rdf-compact-uri (uri)
  "Convert full URI to compact form using known namespaces."
  (let ((result uri))
    (maphash (lambda (prefix namespace-uri)
               (when (string-prefix-p namespace-uri uri)
                 (setq result (concat prefix ":" 
                                     (substring uri (length namespace-uri))))))
             kb-rdf-namespaces)
    result))

;;; RDF/XML Parser

(defun kb-rdf-parse-xml (xml-content)
  "Parse RDF/XML content into an RDF document."
  (let ((xml-tree (with-temp-buffer
                    (insert xml-content)
                    (xml-parse-region (point-min) (point-max))))
        (document (kb-rdf-document-create
                   :namespaces nil
                   :triples nil
                   :format :rdf-xml)))
    
    ;; Parse the XML tree
    (dolist (node xml-tree)
      (when (listp node)
        (kb-rdf-parse-xml-node node document)))
    
    document))

(defun kb-rdf-parse-xml-node (node document)
  "Parse an XML node into RDF triples."
  (let ((tag (xml-node-name node))
        (attributes (xml-node-attributes node))
        (children (xml-node-children node)))
    
    (cond
     ;; RDF root element
     ((eq tag 'rdf:RDF)
      (kb-rdf-extract-namespaces attributes document)
      (dolist (child children)
        (when (listp child)
          (kb-rdf-parse-xml-node child document))))
     
     ;; Resource descriptions
     ((or (eq tag 'rdf:Description)
          (not (string-prefix-p "rdf:" (symbol-name tag))))
      (kb-rdf-parse-resource-node node document))
     
     ;; Skip text nodes and other elements
     (t nil))))

(defun kb-rdf-extract-namespaces (attributes document)
  "Extract namespace declarations from XML attributes."
  (dolist (attr attributes)
    (let ((name (symbol-name (car attr)))
          (value (cdr attr)))
      (when (string-prefix-p "xmlns:" name)
        (let ((prefix (substring name 6)))
          (kb-rdf-add-namespace prefix value)
          (push (kb-rdf-namespace-create :prefix prefix :uri value)
                (kb-rdf-document-namespaces document)))))))

(defun kb-rdf-parse-resource-node (node document)
  "Parse a resource node into RDF triples."
  (let ((tag (xml-node-name node))
        (attributes (xml-node-attributes node))
        (children (xml-node-children node)))
    
    ;; Get subject from rdf:about or generate blank node
    (let ((subject (or (cdr (assq 'rdf:about attributes))
                      (format "_:blank%d" (random 10000)))))
      
      ;; Type triple from element name (unless rdf:Description)
      (unless (eq tag 'rdf:Description)
        (push (kb-rdf-triple-create
               :subject subject
               :predicate (kb-rdf-resolve-uri "rdf:type")
               :object (kb-rdf-resolve-uri (symbol-name tag))
               :object-type :uri)
              (kb-rdf-document-triples document)))
      
      ;; Triples from attributes
      (dolist (attr attributes)
        (unless (memq (car attr) '(rdf:about rdf:ID))
          (push (kb-rdf-triple-create
                 :subject subject
                 :predicate (kb-rdf-resolve-uri (symbol-name (car attr)))
                 :object (cdr attr)
                 :object-type :literal)
                (kb-rdf-document-triples document))))
      
      ;; Triples from child elements
      (dolist (child children)
        (when (listp child)
          (kb-rdf-parse-property-node child subject document))))))

(defun kb-rdf-parse-property-node (node subject document)
  "Parse a property node into an RDF triple."
  (let ((tag (xml-node-name node))
        (attributes (xml-node-attributes node))
        (children (xml-node-children node)))
    
    (let ((predicate (kb-rdf-resolve-uri (symbol-name tag)))
          (object nil)
          (object-type :literal)
          (datatype nil)
          (language nil))
      
      ;; Check for rdf:resource attribute (object property)
      (if-let ((resource (cdr (assq 'rdf:resource attributes))))
          (setq object (kb-rdf-resolve-uri resource)
                object-type :uri)
        ;; Otherwise, get text content
        (setq object (string-trim 
                     (mapconcat (lambda (child)
                                  (if (stringp child) child ""))
                                children "")))
        
        ;; Check for datatype and language
        (setq datatype (cdr (assq 'rdf:datatype attributes)))
        (setq language (cdr (assq 'xml:lang attributes))))
      
      ;; Create and add triple
      (push (kb-rdf-triple-create
             :subject subject
             :predicate predicate
             :object object
             :object-type object-type
             :datatype datatype
             :language language)
            (kb-rdf-document-triples document)))))

;;; Turtle Parser (Basic)

(defun kb-rdf-parse-turtle (turtle-content)
  "Parse Turtle format content into an RDF document."
  (let ((document (kb-rdf-document-create
                   :namespaces nil
                   :triples nil
                   :format :turtle))
        (lines (split-string turtle-content "\n"))
        (current-subject nil))
    
    (dolist (line lines)
      (setq line (string-trim line))
      (unless (or (string-empty-p line) (string-prefix-p "#" line))
        (kb-rdf-parse-turtle-line line document)))
    
    document))

(defun kb-rdf-parse-turtle-line (line document)
  "Parse a single Turtle line."
  (cond
   ;; Namespace declaration
   ((string-match "^@prefix\\s-+\\([^:]+\\):\\s-+<\\([^>]+\\)>\\s-*\\." line)
    (let ((prefix (match-string 1 line))
          (uri (match-string 2 line)))
      (kb-rdf-add-namespace prefix uri)
      (push (kb-rdf-namespace-create :prefix prefix :uri uri)
            (kb-rdf-document-namespaces document))))
   
   ;; Simple triple
   ((string-match "^\\([^\\s-]+\\)\\s-+\\([^\\s-]+\\)\\s-+\\(.+\\)\\s-*\\." line)
    (let ((subject (kb-rdf-resolve-uri (match-string 1 line)))
          (predicate (kb-rdf-resolve-uri (match-string 2 line)))
          (object-str (string-trim (match-string 3 line))))
      
      (let ((object nil)
            (object-type :literal)
            (datatype nil))
        
        (cond
         ;; URI object
         ((string-match "^<\\([^>]+\\)>$" object-str)
          (setq object (match-string 1 object-str)
                object-type :uri))
         
         ;; Qualified name object  
         ((string-match "^\\([^:]+:[^\\s-]+\\)$" object-str)
          (setq object (kb-rdf-resolve-uri (match-string 1 object-str))
                object-type :uri))
         
         ;; Literal with datatype
         ((string-match "^\"\\([^\"]*\\)\"\\^\\^\\(.+\\)$" object-str)
          (setq object (match-string 1 object-str)
                datatype (kb-rdf-resolve-uri (match-string 2 object-str))))
         
         ;; Simple literal
         ((string-match "^\"\\([^\"]*\\)\"$" object-str)
          (setq object (match-string 1 object-str)))
         
         ;; Unquoted literal
         (t (setq object object-str)))
        
        ;; Create triple
        (push (kb-rdf-triple-create
               :subject subject
               :predicate predicate
               :object object
               :object-type object-type
               :datatype datatype)
              (kb-rdf-document-triples document)))))))

;;; Import to Knowledge Base

(defun kb-rdf-import-document (document &optional target-mt)
  "Import an RDF document into the knowledge base."
  (let ((target-mt (or target-mt 'RDFImportMt))
        (imported-count 0))
    
    ;; Ensure target microtheory exists
    (unless (kb-get-microtheory target-mt)
      (kb-create-microtheory target-mt 'CommonSenseMt))
    
    ;; Import each triple
    (dolist (triple (kb-rdf-document-triples document))
      (kb-rdf-import-triple triple target-mt)
      (cl-incf imported-count))
    
    (message "Imported %d triples into microtheory %s" imported-count target-mt)
    imported-count))

(defun kb-rdf-import-triple (triple target-mt)
  "Import a single RDF triple into the knowledge base."
  (let ((subject (kb-rdf-uri-to-symbol (kb-rdf-triple-subject triple)))
        (predicate (kb-rdf-uri-to-symbol (kb-rdf-triple-predicate triple)))
        (object (kb-rdf-convert-object triple)))
    
    (kb-with-microtheory target-mt
      (kb-add-fact subject predicate object))))

(defun kb-rdf-uri-to-symbol (uri)
  "Convert an RDF URI to a Lisp symbol."
  (let ((compact (kb-rdf-compact-uri uri)))
    (intern (replace-regexp-in-string "[^a-zA-Z0-9_-]" "_" compact))))

(defun kb-rdf-convert-object (triple)
  "Convert RDF object to appropriate Lisp value."
  (let ((object (kb-rdf-triple-object triple))
        (object-type (kb-rdf-triple-object-type triple))
        (datatype (kb-rdf-triple-datatype triple)))
    
    (pcase object-type
      (:uri (kb-rdf-uri-to-symbol object))
      (:literal 
       (if datatype
           (kb-rdf-convert-typed-literal object datatype)
         object))
      (:blank-node (intern object))
      (_ object))))

(defun kb-rdf-convert-typed-literal (value datatype)
  "Convert a typed RDF literal to appropriate Lisp type."
  (let ((type-mapping (assoc datatype kb-rdf-type-mapping)))
    (if type-mapping
        (pcase (cdr type-mapping)
          ('integer (string-to-number value))
          ('float (string-to-number value))
          ('boolean (not (member value '("false" "0"))))
          ('datetime value)  ; Keep as string for now
          (_ value))
      value)))

;;; Export from Knowledge Base

(defun kb-rdf-export-microtheory (mt-name &optional format)
  "Export a microtheory to RDF format."
  (let ((mt (kb-get-microtheory mt-name))
        (format (or format :turtle))
        (triples nil))
    
    (unless mt
      (error "Microtheory %s not found" mt-name))
    
    ;; Convert facts to RDF triples
    (maphash (lambda (subject facts)
               (dolist (fact facts)
                 (push (kb-rdf-triple-create
                        :subject (kb-symbol-to-uri subject)
                        :predicate (kb-symbol-to-uri (kb-fact-predicate fact))
                        :object (kb-value-to-rdf-object (kb-fact-object fact))
                        :object-type (if (symbolp (kb-fact-object fact)) :uri :literal))
                       triples)))
             (kb-microtheory-facts mt))
    
    ;; Generate output
    (pcase format
      (:turtle (kb-rdf-serialize-turtle triples))
      (:rdf-xml (kb-rdf-serialize-xml triples))
      (_ (error "Unsupported format: %s" format)))))

(defun kb-symbol-to-uri (symbol)
  "Convert a Lisp symbol to an RDF URI."
  (format "http://kb.local/resource/%s" (symbol-name symbol)))

(defun kb-value-to-rdf-object (value)
  "Convert a Lisp value to RDF object representation."
  (cond
   ((symbolp value) (kb-symbol-to-uri value))
   ((numberp value) (number-to-string value))
   ((stringp value) value)
   (t (format "%S" value))))

;;; Serialization

(defun kb-rdf-serialize-turtle (triples)
  "Serialize RDF triples to Turtle format."
  (let ((output ""))
    ;; Add namespace declarations
    (maphash (lambda (prefix uri)
               (setq output (concat output 
                                   (format "@prefix %s: <%s> .\n" prefix uri))))
             kb-rdf-namespaces)
    (setq output (concat output "\n"))
    
    ;; Add triples
    (dolist (triple triples)
      (setq output (concat output
                          (format "%s %s %s .\n"
                                  (kb-rdf-format-turtle-term 
                                   (kb-rdf-triple-subject triple) :uri)
                                  (kb-rdf-format-turtle-term 
                                   (kb-rdf-triple-predicate triple) :uri)
                                  (kb-rdf-format-turtle-term 
                                   (kb-rdf-triple-object triple)
                                   (kb-rdf-triple-object-type triple))))))
    output))

(defun kb-rdf-format-turtle-term (term type)
  "Format a term for Turtle output."
  (pcase type
    (:uri (format "<%s>" term))
    (:literal (format "\"%s\"" term))
    (_ (format "\"%s\"" term))))

;;; High-level Import Functions

;;;###autoload
(defun kb-rdf-import-file (filename &optional format target-mt)
  "Import RDF data from a file."
  (interactive "fRDF file: ")
  (let ((content (with-temp-buffer
                   (insert-file-contents filename)
                   (buffer-string)))
        (format (or format 
                   (kb-rdf-detect-format filename))))
    
    (let ((document (pcase format
                      (:rdf-xml (kb-rdf-parse-xml content))
                      (:turtle (kb-rdf-parse-turtle content))
                      (_ (error "Unsupported format: %s" format)))))
      
      (kb-rdf-import-document document target-mt))))

;;;###autoload
(defun kb-rdf-import-url (url &optional format target-mt)
  "Import RDF data from a URL."
  (interactive "sRDF URL: ")
  (let ((content (with-current-buffer (url-retrieve-synchronously url)
                   (goto-char (point-min))
                   (search-forward "\n\n")  ; Skip headers
                   (buffer-substring (point) (point-max))))
        (format (or format :rdf-xml)))
    
    (let ((document (pcase format
                      (:rdf-xml (kb-rdf-parse-xml content))
                      (:turtle (kb-rdf-parse-turtle content))
                      (_ (error "Unsupported format: %s" format)))))
      
      (kb-rdf-import-document document target-mt))))

;;;###autoload
(defun kb-rdf-export-to-file (mt-name filename &optional format)
  "Export a microtheory to RDF file."
  (interactive (list (completing-read "Microtheory: " (kb-list-microtheories))
                     (read-file-name "Output file: ")
                     (intern (completing-read "Format: " '("turtle" "rdf-xml") nil t "turtle"))))
  (let ((rdf-content (kb-rdf-export-microtheory (intern mt-name) format)))
    (with-temp-file filename
      (insert rdf-content))
    (message "Exported microtheory %s to %s" mt-name filename)))

(defun kb-rdf-detect-format (filename)
  "Detect RDF format from filename extension."
  (let ((ext (file-name-extension filename)))
    (cond
     ((member ext '("rdf" "owl" "xml")) :rdf-xml)
     ((member ext '("ttl" "turtle")) :turtle)
     ((member ext '("n3")) :n3)
     ((member ext '("nt")) :nt)
     (t :rdf-xml))))

;;; Initialize namespaces on load
(kb-rdf-init-namespaces)

(provide 'kb-rdf)
;;; kb-rdf.el ends here
