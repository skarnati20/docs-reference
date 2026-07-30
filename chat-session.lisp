;;;; chat-session.lisp

(in-package :docs-reference)


(defstruct chat-turn
  "A line in the chat history."
  role
  content
  (chunks nil))

(defstruct chat-session
  "Accumulated chat turns and total chunks used."
  (turns nil)
  (all-chunks nil))

(defun add-query (session query chunks)
  "Add QUERY as a user turn carrying its not-yet-seen CHUNKS, and accumulate
   ALL-CHUNKS as the running union. Returns SESSION."
  (let* ((prev-chunks (chat-session-all-chunks session))
	 (chunk-additions
	   (remove-if
	    (lambda (c) (member c prev-chunks :test #'eq))
	    chunks))
	 (turn (make-chat-turn :role "user" :content query :chunks chunk-additions)))
    (setf (chat-session-all-chunks session)
	  (append prev-chunks chunk-additions))
    (setf (chat-session-turns session)
	  (append (chat-session-turns session)
		  (list turn)))
    session))

(defun register-response (session response)
  "Registers assistant RESPONSE as a turn in SESSION. Returns SESSION."
  (let ((turn (make-chat-turn :role "assistant" :content response)))
    (setf (chat-session-turns session)
	  (append (chat-session-turns session)
		  (list turn)))
    session))
  

(defun format-turn-message (chat-turn)
  "Format CHAT-TURN as an ollama message alist; any CHUNKS are hydrated and
   folded into the content as a context block."
  (let* ((role (chat-turn-role chat-turn))
	 (content (chat-turn-content chat-turn))
	 (chunks (chat-turn-chunks chat-turn))
	 (content-message
	   (if chunks
	       (format nil "Context:~%~A~%~%~A"
		       (format-retrieved-chunks chunks)
		       content)
	       content)))
    (list (cons :|role| (string-downcase (string role)))
	  (cons :|content| content-message))))

(defun chat-session-to-ollama-messages (chat-session)
  "Returns an ollama messages list for CHAT-SESSION to be
   used in an ollama chat request."
  (mapcar #'format-turn-message (chat-session-turns chat-session)))
