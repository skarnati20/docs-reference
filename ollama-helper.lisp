(in-package #:docs-reference)

(defun lisp-to-json-string (data)
  (with-output-to-string (s)
    (json:encode-json data s)))

(defun substitute-subseq (string old new &key (test #'eql))
  "Replace every occurrence of OLD in STRING with NEW."
  (let ((pos (search old string :test test)))
    (if pos
        (concatenate 'string
                     (subseq string 0 pos)
                     new
                     (substitute-subseq (subseq string (+ pos (length old)))
                                        old new :test test))
        string)))
