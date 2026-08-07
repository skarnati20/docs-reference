;;;; search.lisp

(in-package :docs-reference)


;;;; Search Methods


(defparameter *search-methods* '(:dense :bm25 :colbert)
  "Retrievers the search functions can fuse; what :ALL expands to.")

(defparameter *default-search-methods* '(:dense :bm25)
  "Methods used when none are named. ColBERT is opt-in: it costs a second per
   search and needs its own server.")

(defun resolve-search-methods (methods)
  "METHODS as a list. Accepts :ALL, a single keyword, or a list of them."
  (let ((resolved (cond ((eq methods :all) *search-methods*)
			((listp methods) methods)
			(t (list methods)))))
    (when (null resolved)
      (error "No search methods given."))
    (dolist (method resolved resolved)
      (unless (member method *search-methods*)
	(error "Unknown search method ~s; expected one of ~s"
	       method *search-methods*)))))


;;;; Search Functions


(defun search-corpus (corpus query &key (top-k nil)
					(methods *default-search-methods*))
  "Search CORPUS for the TOP-K chunks best matching QUERY, fusing the rankings
   named by METHODS via RRF. Each retriever, and the query embedding it needs,
   is only computed when its method is asked for.
   Returns document-chunk-slim in ranked order (hydrate at formatting time)."
  (let ((methods (resolve-search-methods methods))
	(orders nil))
    (when (member :dense methods)
      (push (order-chunks-by-embedding (corpus-chunks corpus)
				       (run-ollama-embedding query))
	    orders))
    (when (member :bm25 methods)
      (push (order-chunks-by-bm25 (corpus-bm25-chunk-index corpus) query)
	    orders))
    (when (member :colbert methods)
      (push (order-chunks-by-colbert (corpus-chunks corpus)
				     (run-colbert-embeddings query))
	    orders))
    (let ((fused (reciprocal-rank-fusion orders)))
      (if top-k
	  (subseq fused 0 (min top-k (length fused)))
	  fused))))

(defun search-corpora (corpora query &key (top-k nil)
					  (methods *default-search-methods*))
  "Search multiple CORPORA and fuse the rankings named by METHODS via RRF.
   Each retriever, and the query embedding it needs, is only computed when its
   method is asked for.
   Returns document-chunk-slim in ranked order (not hydrated)."
  (let ((methods (resolve-search-methods methods))
	(orders nil))
    (when (member :dense methods)
      (push (order-corpora-chunks-by-embedding corpora
					       (run-ollama-embedding query))
	    orders))
    (when (member :bm25 methods)
      (push (order-corpora-chunks-by-bm25 corpora query)
	    orders))
    (when (member :colbert methods)
      (push (order-corpora-by-colbert corpora (run-colbert-embeddings query))
	    orders))
    (let ((fused (reciprocal-rank-fusion orders)))
      (if top-k
	  (subseq fused 0 (min top-k (length fused)))
	  fused))))
