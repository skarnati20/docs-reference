;;;; fetcher.lisp

(in-package #:docs-reference)

(defun fetch-url-content (url)
  "Fetches and parses the HTML at URL, or NIL when URL is not HTML."
  (if (not (extractable-url-p url))
      nil
      (handler-case
	  (handler-bind
	      ((flexi-streams:external-format-encoding-error
		 (lambda (c)
		   (declare (ignore c))
		   (use-value #\?))))
	    (multiple-value-bind (body status)
		(drakma:http-request url)
	      (cond
		((not (eql status 200)) nil)
		;; Drakma returns a byte vector for non-text content types, and
		;; PLUMP:PARSE only takes a string.
		((not (stringp body)) nil)
		(t (plump:parse body)))))
	(error (e)
	  (format t "Error fetching ~a: ~a~%" url e)
	  nil))))

(defun first-node (nodes)
  "First node of a CLSS result vector, or NIL if the vector is empty."
  (when (plusp (length nodes))
    (aref nodes 0)))

(defparameter *content-selectors*
  "h1, h2, h3, h4, h5, h6, p, li, pre, blockquote, td, th"
  "CSS selector for the text-bearing content tags worth extracting.")

(defun extract-html-text (url)
  "Readable text from the HTML at URL, pulling only content-bearing tags
   (headings, paragraphs, list items, code, etc.) from the main <article>
   or <main> region. This excludes scripts, styles, and nav chrome."
  (let ((dom (fetch-url-content url)))
    (if (null dom)
	""
	(let* ((region (or (first-node (clss:select "article" dom))
			   (first-node (clss:select "main" dom))
			   dom))
	       (nodes (clss:select *content-selectors* region)))
	  (with-output-to-string (out)
	    (loop for node across nodes
		  for text = (string-trim '(#\Space #\Newline #\Tab) (plump:text node))
		  when (plusp (length text))
		    do (write-string text out)
		       (terpri out)))))))

(defun fetch-url-text (url)
  "Text for URL, dispatched on its content kind. Kinds without an extractor
   yield the empty string so an unsupported link cannot break a crawl.
   To support a new kind: add it to *EXTRACTABLE-KINDS* and give it a branch."
  (let ((kind (url-content-kind url)))
    (cond
      ((eq kind :html) (extract-html-text url))
      ;; PDFs are recognised but not extracted yet - drop in a text extractor
      ;; here and add :pdf to *EXTRACTABLE-KINDS* to start indexing them.
      ((eq kind :pdf) "")
      ;; :UNKNOWN - an extension we do not recognise, so assume it is binary.
      (t ""))))

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
    (when parsed-html
      (remove-duplicates
       (loop for node across (clss:select tag parsed-html)
	     for href = (convert-ref-link url (plump:attribute node "href"))
	     when (and href
		       (extractable-url-p href)
		       (or (not same-base) (uiop:string-prefix-p url href)))
	       collect href)
       :test #'string=))))

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
