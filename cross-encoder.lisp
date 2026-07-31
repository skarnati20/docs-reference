;;;; cross-encoder.lisp

(in-package :docs-reference)


(defvar *cross-encoder-endpoint* "http://localhost:8080/rerank")

(defun rerank-texts-request (query texts)
  "Creates a cross-encoder rerank request."
  (let* ((data (list (cons :|query| query)
		     (cons :|documents| (coerce texts 'vector))))
	 (json-data (lisp-to-json-string data)))
    json-data))

(defun score-texts-relevance (query texts)
  "Performs a scoring of TEXTS by their semantic relevance to
   QUERY through llama.cpp's cross-encoder. Returns a list of
   relevance scores in the same order as TEXTS."
  (handler-case
      (let* ((json-data (rerank-texts-request query texts))
	     (response
	       (uiop:run-program
		(list "curl" "-s" *cross-encoder-endpoint*
		      "--data-binary" "@-")
		:input (make-string-input-stream json-data)
		:output :string
		:error-output :string)))
	(with-input-from-string
	    (s response)
	  (let* ((json-as-list (json:decode-json s))
		 (results (cdr (assoc :results json-as-list)))
		 (sorted-results
		   (sort results (lambda (x y)
				   (< (cdr (assoc :index x))
				      (cdr (assoc :index y))))))
		 (sorted-scores (mapcar
				 (lambda (x)
				   (cdr (assoc :relevance--score x)))
				 sorted-results)))
	    sorted-scores)))
    (error (e)
      (format t "Error executing curl command ~a~%" e)
      nil)))

(defun rerank-chunks (query chunks &key (top-k nil))
  "Rerank CHUNKS (document-chunk-slim) by cross-encoder relevance to QUERY.
   Returns a list of (CHUNK . SCORE) conses in ranked order (TOP-K if given).
   Hydrates each chunk's text to score it; on reranker error, falls back to the
   input order with NIL scores."
  (if (null chunks)
      nil
      (let* ((texts (mapcar (lambda (slim)
			      (document-chunk-text (get-full-chunk-from-slim slim)))
			    chunks))
	     (scores (score-texts-relevance query texts))
	     (ranked (if scores
			 (sort (mapcar #'cons chunks scores) #'> :key #'cdr)
			 (mapcar (lambda (c) (cons c nil)) chunks))))
	(if top-k
	    (subseq ranked 0 (min top-k (length ranked)))
	    ranked))))
