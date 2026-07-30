;;;; ollama-embeddings.lisp

(in-package #:docs-reference)

(defvar *ollama-embedding-endpoint* "http://localhost:11434/api/embed")
(defvar *embedding-model-name* "qwen3-embedding:0.6b")

(defun ollama-embedding-request (texts)
  "JSON request body for an embeddings request over a list of TEXTS."
  (let* ((message (list (cons :|model| *embedding-model-name*)
			;; a vector so cl-json emits a JSON array of inputs
			(cons :|input| (coerce texts 'vector))))
	 (json-data (lisp-to-json-string message)))
    (substitute-subseq json-data ":null" ":false" :test #'string=)))

(defun run-ollama-embeddings (texts)
  "Runs an ollama embeddings request over a list of TEXTS and returns a list
of embeddings, one per input, in order."
  (handler-case
      (let* ((json-data (ollama-embedding-request texts))
	     (response
	       ;; Pass curl an argv LIST (uiop runs it directly, with no shell)
	       ;; and feed the JSON body over stdin, so text with backticks,
	       ;; quotes, etc. can never break quoting or inject shell commands.
	       (uiop:run-program
		(list "curl" "-s" *ollama-embedding-endpoint*
		      "--data-binary" "@-")
		:input (make-string-input-stream json-data)
		:output :string
		:error-output :string)))
	(with-input-from-string
	    (s response)
	  (let* ((json-as-list (json:decode-json s))
		 (embeddings (cdr (assoc :embeddings json-as-list))))
	    embeddings)))
    (error (e)
      (format t "Error executing curl command ~a~%" e)
      nil)))

(defun run-ollama-embedding (text)
  "Runs an ollama embeddings request on TEXT and returns the embedding."
  (first (run-ollama-embeddings (list text))))
