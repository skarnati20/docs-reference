;;;; local-storage.lisp

(in-package :docs-reference)


(defparameter *storage-path* (uiop:xdg-cache-home "docs-reference/"))

(defparameter *embeddings-format-version* 1
  "Bumped when the on-disk layout changes, so older files are refused by
   READ-EMBEDDINGS rather than misread.")

(defun single-to-u32 (x)
  "The bits of single-float X as an unsigned 32-bit word."
  (ldb (byte 32 0) (sb-kernel:single-float-bits x)))

(defun u32-to-single (word)
  "The single-float whose bits are the unsigned 32-bit WORD."
  (sb-kernel:make-single-float
   (if (>= word #x80000000)
       (- word #x100000000)
       word)))

(defun cache-path (filename &key (path *storage-path*))
  "Full path to the cache file named FILENAME. Single place that knows how a
   name and the storage root are joined."
  (uiop:subpathname (uiop:ensure-directory-pathname path) filename))

(defun store-embeddings (embeddings filename &key (path *storage-path*))
  "Stores EMBEDDINGS at PATH. Returns the file written, or NIL when EMBEDDINGS
   is empty - a failed embedding must never be cached, or the empty result
   would be served forever in place of a retry."
  (when embeddings
    (let ((file (cache-path filename :path path))
	  (n-tokens (length embeddings))
	  (dim (length (the embedding (first embeddings)))))
      (ensure-directories-exist file)
      (with-open-file (out file :direction :output
				:element-type '(unsigned-byte 32)
				:if-exists :supersede
				:if-does-not-exist :create)
	(write-byte *embeddings-format-version* out)
	(write-byte n-tokens out)
	(write-byte dim out)
	(dolist (embedding embeddings)
	  (loop for x across (the embedding embedding)
		do (write-byte (single-to-u32 x) out))))
      file)))

(defun read-embeddings (filename &key (path *storage-path*))
  "Reads EMBEDDINGS from PATH. Returns a list of EMBEDDINGs, or NIL when the
   file is missing, written by another format version, or truncated - callers
   treat every one of those the same way, by recomputing."
  (let ((file (cache-path filename :path path)))
    (when (probe-file file)
      (handler-case
	  (with-open-file (in file :element-type '(unsigned-byte 32))
	    (let ((version (read-byte in nil nil))
		  (n-tokens (read-byte in nil nil))
		  (dim (read-byte in nil nil)))
	      (when (and version n-tokens dim
			 (eql version *embeddings-format-version*))
		(loop repeat n-tokens
		      collect (let ((embedding (make-embedding dim)))
				(dotimes (i dim embedding)
				  (setf (aref embedding i)
					(u32-to-single (read-byte in)))))))))
	(error () nil)))))

(defun store-text (text filename &key (path *storage-path*))
  "Stores TEXT at PATH. Returns the file written, or NIL for empty TEXT -
   FETCH-URL-TEXT yields \"\" for a failed fetch or an unsupported content
   type, and caching that would serve emptiness in place of a retry."
  (when (plusp (length text))
    (let ((file (cache-path filename :path path)))
      (ensure-directories-exist file)
      (with-open-file (out file :direction :output
				:if-exists :supersede
				:if-does-not-exist :create
				:external-format :utf-8)
	(write-string text out))
      file)))

(defun read-text (filename &key (path *storage-path*))
  "Reads text from PATH, or NIL when the file is missing or unreadable."
  (let ((file (cache-path filename :path path)))
    (when (probe-file file)
      (handler-case
	  (uiop:read-file-string file :external-format :utf-8)
	(error () nil)))))
