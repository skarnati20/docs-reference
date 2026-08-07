;; docs-reference.asd

(asdf:defsystem #:docs-reference
  :description "Library for referencing online docs"
  :author "Sai Karnati"
  :license "Apache 2"
  :depends-on (#:uiop #:cl-json #:drakma #:plump #:clss #:lparallel)
  :components ((:file "package")
	       (:file "stream")
	       (:file "content-types")
	       (:file "fetcher")
	       (:file "helpers")
	       (:file "vector")
	       (:file "local-storage")
	       (:file "lemmatization")
	       (:file "colbert")
	       (:file "ollama-embeddings")
	       (:file "store")
	       (:file "search")
	       (:file "ollama-chat")
	       (:file "rag")
	       (:file "chat-session")
	       (:file "docs-reference")
	       (:file "bm25")
	       (:file "scoring")
	       (:file "eval")
	       (:file "cross-encoder")))
