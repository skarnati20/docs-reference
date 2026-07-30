;;;; ollama-chat.lisp

(in-package #:docs-reference)

(defvar *ollama-chat-endpoint* "http://localhost:11434/api/chat")
(defvar *chat-model-name* "qwen3:1.7b")

(defun ollama-chat-request (messages &key thinking)
  "JSON request body for a chat completion over MESSAGES (a list of ollama
   message alists). When THINKING is nil, adds think:false to disable the
   model's reasoning mode."
  (let* ((data (append (list (cons :|model| *chat-model-name*)
                             (cons :|stream| nil))
                       (unless thinking (list (cons :|think| nil)))
                       (list (cons :|messages| messages))))
	 (json-data (lisp-to-json-string data)))
    (substitute-subseq json-data ":null" ":false" :test #'string=)))

(defun run-ollama-chat-json (json-data)
  "Send JSON-DATA to the ollama chat endpoint and return the assistant message
   content string, or NIL on error."
  (handler-case
      (let ((response
	      ;; argv LIST (no shell) + JSON over stdin, so prompt text with
	      ;; backticks, quotes, etc. can't break quoting or inject shell.
	      (uiop:run-program
	       (list "curl" "-s" *ollama-chat-endpoint*
		     "--data-binary" "@-")
	       :input (make-string-input-stream json-data)
	       :output :string
	       :error-output :string)))
	(with-input-from-string
	    (s response)
	  (let* ((json-as-list (json:decode-json s))
		 (message (cdr (assoc :message json-as-list)))
		 (content (cdr (assoc :content message))))
	    content)))
    (error (e)
      (format t "Error executing curl command ~a~%" e)
      nil)))

(defun run-ollama-chat (prompt &key messages thinking)
  "Run a chat request and return the assistant response. PROMPT is the current
   user turn; MESSAGES is prior history (appended before PROMPT, which becomes
   the final user message). THINKING enables the model's reasoning mode."
  (let ((msgs (append messages
		      (list (list (cons :|role| "user")
				  (cons :|content| prompt))))))
    (run-ollama-chat-json (ollama-chat-request msgs :thinking thinking))))
