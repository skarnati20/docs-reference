;;;; colbert.lisp

(in-package :docs-reference)


(defvar *colbert-embedding-endpoint* "http://localhost:8081/embedding")

(defun colbert-embeddings-request (query)
  (let* ((data (list (cons :|content| query)))
	 (json-data (lisp-to-json-string data)))
    json-data))

(defun colbert-response-embeddings (json)
  "Per-token embeddings from a decoded llama.cpp /embedding response. The
   endpoint returns an ARRAY of results (one per input), each an object whose
   :embedding holds one vector per token - so the result object has to be
   unwrapped before looking up :embedding."
  (let ((result (if (keywordp (car (first json)))
		    json          ; already a single result object
		    (first json))))
    (cdr (assoc :embedding result))))

(defun run-colbert-embeddings (query)
  "Runs a ColBERT embeddings request on QUERY. Returns one UNIT embedding per
   token (requires the server to run with --pooling none), or NIL on error.
   The vectors are L2-normalized here so MAX-SIM's dot product is a cosine -
   raw ColBERT vectors have magnitudes around 25, which would otherwise swamp
   the similarity signal."
  (handler-case
      (let* ((json-data (colbert-embeddings-request query))
	     (response
	       (uiop:run-program
		(list "curl" "-s" *colbert-embedding-endpoint*
		      "--data-binary" "@-")
		:input (make-string-input-stream json-data)
		:output :string
		:error-output :string)))
	(with-input-from-string
	    (s response)
	  (mapcar (lambda (v) (make-unit (to-embedding v)))
		  (colbert-response-embeddings (json:decode-json s)))))
    (error (e)
      (format t "Error executing curl command ~a~%" e)
      nil)))

(defun max-sim (query-embedding docs-embeddings)
  "Returns max similary of QUERY-TOKEN against DOCS-TOKEN.
   Both must be unit vectors, so the dot product is a cosine. Yields 0 when
   DOCS-EMBEDDINGS is empty, rather than erroring the way REDUCE would."
  (loop for doc-embedding in docs-embeddings
	maximize (dot-product query-embedding doc-embedding)))

(defun score-colbert-embeddings (query-embeddings doc-embeddings)
  "Returns the ColBERT score between QUERY-EMBEDDINGS
   and DOC-EMBEDDINGS."
  (reduce #'+ (mapcar (lambda (x) (max-sim x doc-embeddings))
			 query-embeddings)))

(defun score-colbert-query-and-doc (query doc)
  "Returns the ColBERT score between QUERY and DOCS."
  (let* ((query-embeddings (run-colbert-embeddings query))
	 (doc-embeddings (run-colbert-embeddings doc)))
    (score-colbert-embeddings query-embeddings doc-embeddings)))
    

