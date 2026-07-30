;;;; vector.lisp

(in-package :docs-reference)


(defun vector-centroid (vectors)
  "Element-wise mean of a list of numeric vectors (each a list of numbers).
   Returns NIL if VECTORS is empty."
  (when vectors
    (let ((n (length vectors)))
      (mapcar (lambda (sum) (/ sum n))
	      (reduce (lambda (acc v) (mapcar #'+ acc v))
		      (rest vectors)
		      :initial-value (first vectors))))))

(defun dot-product (vec-a vec-b)
  "Compute the dot product of two numeric lists."
  (loop for a in vec-a
	for b in vec-b
	sum (* a b)))

(defun vector-magnitude (vec)
  "Compute the magnitude (L2 norm) of a numeric list."
  (sqrt (loop for x in vec sum (* x x))))

(defun cosine-similarity (vec-a vec-b)
  "Compute cosine similarity between two embedding vectors.
   Returns a value between -1 and 1."
  (let ((mag-a (vector-magnitude vec-a))
	(mag-b (vector-magnitude vec-b)))
    (if (or (zerop mag-a) (zerop mag-b))
	0.0
	(/ (dot-product vec-a vec-b) (* mag-a mag-b)))))
