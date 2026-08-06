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
	       (:file "ollama-helper")
	       ;; vector, local-storage, lemmatization and colbert define
	       ;; primitives that ollama-embeddings and store call, so they
	       ;; load first. colbert only handles text and raw vectors, never
	       ;; the chunk structs, so it can sit below store without a cycle.
	       (:file "vector")
	       (:file "local-storage")
	       (:file "lemmatization")
	       (:file "colbert")
	       (:file "ollama-embeddings")
	       (:file "store")
	       (:file "search")
	       (:file "ollama-chat")
	       (:file "agents")
	       (:file "chat-session")
	       (:file "docs-reference")
	       (:file "bm25")
	       (:file "scoring")
	       (:file "eval")
	       (:file "cross-encoder")))
