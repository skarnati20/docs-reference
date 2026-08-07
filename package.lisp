;;;; package.lisp

(defpackage #:docs-reference
  (:use #:cl)
  ;; Registration
  (:export #:register-link
           #:register-link-async)
  ;; Search
  (:export #:find-docs
           #:docs-search
           #:docs-search-async
           #:docs-chat
           #:docs-chat-async)
  ;; Short names
  (:export #:rl
           #:rla
           #:ds
           #:dsa
           #:dc
           #:dca)
  ;; Printing
  (:export #:print-links
           #:print-corpora)
  ;; Cleanup
  (:export #:remove-link
           #:remove-link-idx
           #:clear-docs
           #:clear-chat))
