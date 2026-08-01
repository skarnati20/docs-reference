;;;; content-types.lisp

(in-package :docs-reference)


(defparameter *kind-by-extension*
  '((".html" . :html)
    (".htm"  . :html)
    (".xhtml" . :html)
    (".txt"  . :html)
    (".md"   . :html)
    (".pdf"  . :pdf))
  "Extensions we recognise, mapped to their content kind. A URL with no
   extension is assumed to be :HTML; any other extension is :UNKNOWN.")

(defparameter *extractable-kinds* '(:html)
  "Content kinds we can currently turn into text. To start indexing a new kind,
   add it here and give it a branch in FETCH-URL-TEXT.")

(defun url-path (url)
  "The path portion of URL, without its query string or fragment."
  (or (ignore-errors (puri:uri-path (puri:parse-uri url))) url))

(defun url-extension (url)
  "The lowercase extension of URL's last path segment."
  (let* ((path (string-downcase (url-path url)))
	 (slash (position #\/ path :from-end t))
	 (name (if slash (subseq path (1+ slash)) path))
	 (dot (position #\. name :from-end t)))
    (when (and dot (plusp dot) (< dot (1- (length name))))
      (let ((extension (subseq name dot)))
	(when (and (<= (length extension) 6)
		   (some #'alpha-char-p extension)
		   (every #'alphanumericp (subseq extension 1)))
	  extension)))))

(defun url-content-kind (url)
  "The kind of content URL points at. No extension means :HTML (most doc
   pages); a recognised extension gives its kind; anything else is :UNKNOWN."
  (let ((extension (url-extension url)))
    (cond
      ((null extension) :html)
      ((cdr (assoc extension *kind-by-extension* :test #'string=)))
      (t :unknown))))

(defun extractable-url-p (url)
  "True when URL points at a content kind we can extract text from, so it is
   worth fetching and crawling."
  (and (member (url-content-kind url) *extractable-kinds*) t))
