;;;; package.lisp

(defpackage #:docs-reference
  (:use #:cl)
  (:export #:register-base-link)
  (:export #:docs-search)
  (:export #:docs-search-direct)
  (:export #:print-corpora))
