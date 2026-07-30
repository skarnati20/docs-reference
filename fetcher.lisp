;;;; fetcher.lisp

(in-package #:docs-reference)

(defun fetch-url-content (url)
  "Fetches and parses the HTML at URL. Bytes that aren't valid in the
   response's declared encoding are replaced with #\\? instead of signaling,
   so one malformed page cannot abort a crawl."
  (handler-bind
      ((flexi-streams:external-format-encoding-error
	 (lambda (c)
	   (declare (ignore c))
	   (use-value #\?))))
    (let* ((html-content (drakma:http-request url))
	   (parsed-html (plump:parse html-content)))
      parsed-html)))

(defun first-node (nodes)
  "First node of a CLSS result vector, or NIL if the vector is empty."
  (when (plusp (length nodes))
    (aref nodes 0)))

(defparameter *content-selectors*
  "h1, h2, h3, h4, h5, h6, p, li, pre, blockquote, td, th"
  "CSS selector for the text-bearing content tags worth extracting.")

(defun fetch-url-text (url)
  "Fetches readable text from a URL by pulling only content-bearing tags
   (headings, paragraphs, list items, code, etc.) from the main <article>
   or <main> region. This excludes scripts, styles, and nav chrome."
  (let* ((dom (fetch-url-content url))
	 (region (or (first-node (clss:select "article" dom))
		     (first-node (clss:select "main" dom))
		     dom))
	 (nodes (clss:select *content-selectors* region)))
    (with-output-to-string (out)
      (loop for node across nodes
	    for text = (string-trim '(#\Space #\Newline #\Tab) (plump:text node))
	    when (plusp (length text))
	      do (write-string text out)
		 (terpri out)))))

(defun trim-final (s char)
  "Return S without its last character if that character is CHAR."
  (if (and (plusp (length s))
	   (char= (char s (1- (length s))) char))
      (subseq s 0 (1- (length s)))
      s))

(defun url-origin (url)
  "Return the scheme://host portion of URL, with no trailing slash."
  (let* ((mark (search "//" url))
	 (host-start (if mark (+ mark 2) 0))
	 (slash (position #\/ url :start host-start)))
    (if slash (subseq url 0 slash) url)))

(defun convert-ref-link (base-url url)
  "Resolve URL (possibly relative: ../, ./, plain, root-relative, or already
   absolute) against BASE-URL into an absolute URL string, per RFC 3986.
   Any #fragment is dropped. Returns NIL if URL is NIL or unparseable."
  (when url
    (handler-case
	(let ((merged (puri:merge-uris url base-url)))
	  (setf (puri:uri-fragment merged) nil)
	  (puri:render-uri merged nil))
      (error () nil))))

(defun fetch-url-links (url &key (tag "a") (same-base t))
  "Fetches links referenced in existing link."
  (let ((parsed-html (fetch-url-content url)))
    (remove-duplicates
     (loop for node across (clss:select tag parsed-html)
	   for href = (convert-ref-link url (plump:attribute node "href"))
	   when (and href
		     (or (not same-base) (uiop:string-prefix-p url href)))
	     collect href)
     :test #'string=)))

(defun fetch-url-links-recursive (start-url &key (depth 3) (tag "a") (same-base t))
  "Fetches links referenced in existing link (recursive till depth)."
  (let ((seen '()))
    (labels ((walk (url d)
	       (when (and (> d 0)
			  (not (member url seen :test #'string=)))
		 (push url seen)
		 (dolist (child (fetch-url-links url :tag tag :same-base same-base))
		   (walk child (1- d))))))
      (walk start-url depth)
      (nreverse seen))))
