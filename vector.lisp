;;;; vector.lisp
;;;;
;;;; Embeddings are (simple-array single-float (*)) rather than lists: packed
;;;; storage costs 4 bytes an element instead of a 16-byte cons plus a 16-byte
;;;; boxed float, and the declarations below let SBCL emit unboxed float
;;;; arithmetic in the inner loops. Vectors are coerced at the two points they
;;;; enter the system, RUN-OLLAMA-EMBEDDINGS and RUN-COLBERT-EMBEDDINGS.

(in-package :docs-reference)


(deftype embedding ()
  "A dense vector of single-floats."
  '(simple-array single-float (*)))

(defun make-embedding (n)
  "A zeroed embedding of length N."
  (make-array n :element-type 'single-float :initial-element 0.0))

(defun to-embedding (numbers)
  "Coerce NUMBERS (a list or vector) into an EMBEDDING."
  (if (typep numbers 'embedding)
      numbers
      (map 'embedding (lambda (x) (float x 1.0)) numbers)))

(defun vector-centroid (vectors)
  "Element-wise mean of a list of embeddings. Returns NIL if VECTORS is empty."
  (when vectors
    (let* ((n (length vectors))
	   (dim (length (the embedding (first vectors))))
	   (acc (make-embedding dim)))
      (declare (type embedding acc))
      (dolist (v vectors)
	(let ((v (the embedding v)))
	  (dotimes (i dim)
	    (incf (aref acc i) (aref v i)))))
      (dotimes (i dim acc)
	(setf (aref acc i) (/ (aref acc i) n))))))

(defun dot-product (vec-a vec-b)
  "Compute the dot product of two embeddings."
  (declare (type embedding vec-a vec-b)
	   (optimize (speed 3) (safety 1)))
  (let ((sum 0.0))
    (declare (type single-float sum))
    (dotimes (i (min (length vec-a) (length vec-b)) sum)
      (incf sum (* (aref vec-a i) (aref vec-b i))))))

(defun vector-magnitude (vec)
  "Compute the magnitude (L2 norm) of an embedding."
  (declare (type embedding vec)
	   (optimize (speed 3) (safety 1)))
  (let ((sum 0.0))
    (declare (type single-float sum))
    (dotimes (i (length vec))
      (incf sum (* (aref vec i) (aref vec i))))
    (sqrt sum)))

(defun cosine-similarity (vec-a vec-b)
  "Compute cosine similarity between two embeddings.
   Returns a value between -1 and 1."
  (let ((mag-a (vector-magnitude vec-a))
	(mag-b (vector-magnitude vec-b)))
    (if (or (zerop mag-a) (zerop mag-b))
	0.0
	(/ (dot-product vec-a vec-b) (* mag-a mag-b)))))

(defun make-unit (vec)
  "Makes VEC a unit vector. A zero vector has no direction, so it is returned
   unchanged rather than dividing by zero."
  (declare (type embedding vec))
  (let ((mag (vector-magnitude vec)))
    (if (zerop mag)
	vec
	(let ((unit (make-embedding (length vec))))
	  (dotimes (i (length vec) unit)
	    (setf (aref unit i) (/ (aref vec i) mag)))))))
