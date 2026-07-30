;;;; scoring.lisp

(in-package :docs-reference)


;;;; Document Ordering


(defun order-chunks-by-embedding (chunks query-embedding)
  "Orders CHUNKS by cosine similarity to QUERY-EMBEDDING (precomputed)."
  (let ((scored-chunks
	  (loop for chunk in chunks
		collect (cons chunk
			      (cosine-similarity query-embedding
						 (document-chunk-slim-embedding chunk))))))
    (mapcar #'car (sort scored-chunks #'> :key #'cdr))))

(defun order-corpora-chunks-by-embedding (corpora query-embedding)
  "Orders chunks in CORPORA by cosine similarity to QUERY-EMBEDDING (precomputed)."
  (let ((chunks
	  (loop for corpus in corpora
		append (corpus-chunks corpus))))
    (order-chunks-by-embedding chunks query-embedding)))
  

(defun order-chunks-by-bm25 (bm25-index query)
  "Orders chunks by bm25 ranking algorithm to QUERY text."
  (order-docs bm25-index (tokenize-text query)))

(defun order-corpora-chunks-by-bm25 (corpora query)
  "Orders chunks in corpora by a combined bm25 ranking to QUERY text."
  (let ((chunk-index
	  (full-chunk-index-from-corpora corpora)))
    (order-chunks-by-bm25 chunk-index query)))

(defun reciprocal-rank-fusion (orders &key (k 60))
  "Implements the RRF algorithm which takes an ranked
   set of documents and returns a final combined ranking."
  (let ((score-map (make-hash-table :test 'eq)))
    (dolist (order orders)
      (loop for item in order
	    for rank from 1
	    do (incf (gethash item score-map 0.0)
		     (/ 1.0 (+ k rank)))))
    (let ((pairs nil))
      (maphash (lambda (k v) (push (cons k v) pairs))
	       score-map)
      (mapcar #'car (sort pairs #'> :key #'cdr)))))
