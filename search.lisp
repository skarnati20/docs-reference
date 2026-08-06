;;;; search.lisp

(in-package :docs-reference)


;;;; Search Functions


(defun search-corpus (corpus query &key (top-k nil))
  "Search CORPUS for the TOP-K chunks best matching QUERY, fusing dense
   similarity and BM25 keyword ranking (over QUERY string) via RRF.
   Returns document-chunk-slim in ranked order (hydrate at formatting time)."
  (let* ((query-embedding (run-ollama-embedding query))
	 (embedding-order
	   (order-chunks-by-embedding (corpus-chunks corpus) query-embedding))
	 (bm25-order
	   (order-chunks-by-bm25 (corpus-bm25-chunk-index corpus) query))
	 (colbert-order
	   (order-chunks-by-colbert (corpus-chunks corpus)
				    (run-colbert-embeddings query)))
	 (fused
	   (reciprocal-rank-fusion
	    (list embedding-order bm25-order colbert-order))))
    (if top-k
	(subseq fused 0 (min top-k (length fused)))
	fused)))

(defun search-corpora (corpora query &key (top-k nil))
  "Search multiple CORPORA and fuse their per-corpus hybrid results with RRF.
   Embeds QUERY once and reuses that embedding across all corpora.
   Returns document-chunk-slim in ranked order (not hydrated)."
  (let* ((query-embedding (run-ollama-embedding query))
	 (embedding-order
	   (order-corpora-chunks-by-embedding corpora query-embedding))
	 (bm25-order
	   (order-corpora-chunks-by-bm25 corpora query))
	 (colbert-order
	   (order-corpora-by-colbert corpora (run-colbert-embeddings query)))
	 (fused
	   (reciprocal-rank-fusion
	    (list embedding-order bm25-order colbert-order))))
    (if top-k
	(subseq fused 0 (min top-k (length fused)))
	fused)))
