;; docs-reference.asd

(asdf:defsystem #:docs-reference
  :description "Library for referencing online docs"
  :author "Sai Karnati"
  :license "Apache 2"
  :depends-on (#:uiop #:cl-json #:drakma #:plump #:clss #:lparallel)
  :components ((:file "package")
	       (:file "stream")
	       (:file "fetcher")
	       (:file "ollama-helper")
	       (:file "ollama-embeddings")
	       (:file "store")
	       (:file "search")
	       (:file "vector")
	       (:file "ollama-chat")
	       (:file "agents")
	       (:file "chat-session")
	       (:file "docs-reference")
	       (:file "bm25")
	       (:file "scoring")
	       (:file "eval")
	       (:file "cross-encoder")
	       (:file "lemmatization")))
