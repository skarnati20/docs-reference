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

(defun rank-chunks-in-order (query chunks &key (top-k nil))
  "Ranks CHUNKS by revelance to QUERY but does not sort them."
  (if (null chunks)
      nil
      (let* ((texts (mapcar (lambda (slim)
			      (document-chunk-text (get-full-chunk-from-slim slim)))
			    chunks))
	     (scores (score-texts-relevance query texts))
	     ;; MAPCAR stops at the shortest list, so a NIL scores list (reranker
	     ;; error) would silently yield no chunks at all; pair with NIL instead.
	     (ranked (if scores
			 (mapcar #'cons chunks scores)
			 (mapcar (lambda (c) (cons c nil)) chunks))))
	(if top-k
	    (subseq ranked 0 (min top-k (length ranked)))
	    ranked))))
	     
(defun rerank-chunks (query chunks &key (top-k nil))
  "Rerank CHUNKS (document-chunk-slim) by cross-encoder relevance to QUERY.
   Returns a list of (CHUNK . SCORE) conses in ranked order (TOP-K if given).
   Hydrates each chunk's text to score it; on reranker error, falls back to the
   input order with NIL scores."
  (let ((ranked-chunks (rank-chunks-in-order query chunks)))
    (if (null ranked-chunks)
	nil
	(let ((reranked
		;; A NIL score means the reranker errored: keep the input
		;; (hybrid) order rather than sorting on NIL.
		(if (cdr (first ranked-chunks))
		    (sort ranked-chunks #'> :key #'cdr)
		    ranked-chunks)))
	  (if top-k
	      (subseq reranked 0 (min top-k (length reranked)))
	      reranked)))))

(defun rerank-window (query chunks &key (start 0) (end nil))
  "CHUNKS with the slice [START, END) reordered by cross-encoder relevance to
   QUERY. Ranks before START keep their position, so a strong head cannot be
   demoted; ranks from END on are left untouched. END defaults to the end of
   CHUNKS."
  (let* ((len (length chunks))
	 (start (min start len))
	 (end (max start (min (or end len) len)))
	 (window (subseq chunks start end)))
    (if (null window)
	chunks
	(append (subseq chunks 0 start)
		(mapcar #'car (rerank-chunks query window))
		(subseq chunks end)))))
