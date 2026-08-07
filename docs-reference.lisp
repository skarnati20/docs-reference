;;;; docs-reference.lisp

(in-package :docs-reference)


(defvar *links* nil)
(defvar *corpora* nil)
(defvar *corpora-lock* (bt:make-lock "corpora"))
(defvar *chat-session* (make-chat-session))

;;;; Registration


(defun register-link (base-url)
  "Registers a link for retrieval. Returns registered links."
  (ensure-kernel)
  (let* ((all-links (fetch-url-links-recursive base-url))
         (new-corpora (remove nil (lparallel:pmapcar #'make-corpus-from-url all-links))))
    (bt:with-lock-held (*corpora-lock*)
      (setf *corpora* (append new-corpora *corpora*))
      (pushnew base-url *links* :test #'string=))
    all-links))

(defun register-link-async (base-url)
  "Registers a link for retrieval in background. Returns as TASK."
  (in-background (:name (format nil "index ~a" base-url))
    (let ((links (register-link base-url)))
      (format t "~{~a~%~}" links))))


;;;; Search


(defun find-docs (query &key (top-k 10))
  "Searches docs only (no chat response)."
  (format-retrieved-chunks
   (search-corpora *corpora* query :top-k top-k)))

(defun docs-search (query &rest options)
  "Searches by doing a direct search on all of *CORPORA*"
  (car (apply #'run-rag *corpora* query options)))

(defun docs-search-async (query &rest options)
  "Searches by doing a direct search on all of *CORPORA*
   in the background."
  (in-background (:name (format nil "search ~a" query))
    (let ((answer (apply #'docs-search query options)))
      (format t "~%=== ANSWER (~a) ===~%~a~%" query answer)
      answer)))

(defun docs-chat (query &rest options)
  "Answer QUERY over *CORPORA* with rolling chat history in *CHAT-SESSION*.
   Returns the answer string."
  (unless *chat-session*
    (setf *chat-session* (make-chat-session)))
  (let* ((session *chat-session*)
	 (history (chat-session-to-ollama-messages session))
	 (result (apply #'run-rag *corpora* query
			:history history
			:past-chunks (chat-session-all-chunks session)
			options))
	 (answer (car result))
	 (chunks (cdr result)))
    (add-query session query chunks)
    (register-response session answer)
    answer))

(defun docs-chat-async (query &rest options)
  "Runs DOCS-CHAT in the background, keeping the rolling chat history."
  (in-background (:name (format nil "chat ~a" query))
    (let ((answer (apply #'docs-chat query options)))
      (format t "~%=== ANSWER (~a) ===~%~a~%" query answer)
      answer)))


;;;; Short Names for Functions


(defun rl (base-url)
  (register-link base-url))

(defun rla (base-url)
  (register-link-async base-url))

(defun ds (query &rest options)
  (apply #'docs-search query options))

(defun dsa (query &rest options)
  (apply #'docs-search-async query options))

(defun dc (query &rest options)
  (apply #'docs-chat query options))

(defun dca (query &rest options)
  (apply #'docs-chat-async query options))


;;;; Printing


(defun print-links ()
  "Prints the list of links available for retrieval."
  (let ((count 0))
    (dolist (link *links*)
      (progn
	(format t "~A URL: ~A~%"
		count
		link)
	(incf count)))))

(defun print-corpora ()
  "Print the name and chunk count of each corpus in *corpora*."
  (dolist (corpus *corpora*)
    (format t "~A (~A chunks)~%"
	    (corpus-name corpus)
	    (length (corpus-chunks corpus)))))


;;;; Cleanup


(defun remove-link (link)
  (let ((new-link-list (remove-if
			(lambda (x) (string= x link))
			*links*))
	(new-corpora (remove-if
		      (lambda (x) (uiop:string-prefix-p link (corpus-name x)))
		      *corpora*)))
    (setf *links* new-link-list)
    (setf *corpora* new-corpora)))

(defun remove-link-idx (link-idx)
  (let ((link (nth link-idx *links*)))
    (remove-link link)))

(defun clear-docs ()
  "Clears the exisiting *CORPORA*"
  (progn
    (setf *links* nil)
    (setf *corpora* nil)))
	 
(defun clear-chat ()
  "Clears the existing *CHAT-HISTORY*"
  (setf *chat-session* (make-chat-session)))
