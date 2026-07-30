;;;; bm25.lisp
;;;; NOTE: Taken from Mark Watson's Loving Common Lisp

(in-package #:docs-reference)


(defclass bm25-index ()
  ((doc-freqs :initarg :doc-freqs :reader doc-freqs)
   (doc-lengths :initarg :doc-lengths :reader doc-lengths)
   (avg-doc-length :initarg :avg-doc-length :reader avg-doc-length)
   (corpus-size :initarg :corpus-size :reader corpus-size)
   (corpus :initarg :corpus :reader corpus)
   (documents :initarg :documents :reader documents)
   (k1 :initarg :k1 :reader k1)
   (b :initarg :b :reader b)))

(defun make-bm25-index (documents &key (key #'identity) (k1 1.5) (b 0.75))
  "Creates a BM25 index over DOCUMENTS. KEY maps a document to its token list
   (a list of term strings); it defaults to IDENTITY so plain token lists can
   still be passed directly. The index keeps DOCUMENTS so ORDER-DOCS returns
   the stored objects (chunks, corpora, ...) themselves."
  (let* ((tokenized-corpus (mapcar key documents))
	 (corpus-size (length documents))
	 (doc-lengths (mapcar #'length tokenized-corpus))
	 (avg-doc-length (if (zerop corpus-size)
			     0
			     (/ (reduce #'+ doc-lengths) corpus-size)))
	 (doc-freqs (make-hash-table :test 'equal)))
    (dolist (doc tokenized-corpus)
      (dolist (term (remove-duplicates doc :test #'string=))
	(incf (gethash term doc-freqs 0))))
    (make-instance 'bm25-index
		   :doc-freqs doc-freqs
		   :doc-lengths doc-lengths
		   :avg-doc-length avg-doc-length
		   :corpus-size corpus-size
		   :corpus tokenized-corpus
		   :documents documents
		   :k1 k1
		   :b b)))

(defmethod order-docs ((index bm25-index) query-tokens)
  "Returns the stored documents ordered by BM25 score for QUERY-TOKENS,
   most relevant first."
  (let ((scored (loop for doc in (documents index)
		      for i from 0
		      collect (cons doc (score-doc index query-tokens i)))))
    (mapcar #'car (sort scored #'> :key #'cdr))))


;;;; Internal methods


(defmethod inverse-document-frequency ((index bm25-index) term)
  "Calculates the IDF for a given term."
  (let* ((doc-freq (gethash term (doc-freqs index) 0))
	 (corpus-size (corpus-size index)))
    (log (+ 1 (/ (+ (- corpus-size doc-freq) 0.5) (+ doc-freq 0.5))) 10)))

(defmethod score-doc ((index bm25-index) query-tokens doc-index)
  "Calculates the BM25 score for a single document."
  (let* ((k1 (k1 index))
	 (b (b index))
	 (doc-length (nth doc-index (doc-lengths index)))
	 (doc (nth doc-index (corpus index)))
	 (avg-dl (avg-doc-length index))
	 (score 0.0))
    (dolist (term query-tokens)
      (let* ((term-freq (count term doc :test #'string=))
	     (idf (inverse-document-frequency index term)))
	(incf score (* idf (/ (* term-freq (+ k1 1))
			      (+ term-freq
				 (* k1
				    (+ (- 1 b)
				       (* b
					  (/
					   doc-length
					   avg-dl))))))))))
    score))
