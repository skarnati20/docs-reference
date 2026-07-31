;;;; store.lisp

(in-package #:docs-reference)


(defstruct chunk-with-offsets
  "A chunk of text with its text, and offsets in url content."
  text
  start-offset
  end-offset)

(defstruct document-chunk-slim
  "A chunk of text with its url, text offsets, and embedding vector."
  url
  start-offset
  end-offset
  embedding
  tokens)

(defstruct document-chunk
  "A hydrated chunk: url and text, for LLM context."
  url
  text)

(defstruct corpus
  "A named collection of document chunks for retrieval."
  name
  embedding
  keywords
  bm25-chunk-index
  (chunks nil))



(defvar *default-chunk-size* 500)
(defvar *chunk-overlap* 50)


;;;; Processing


(defparameter *token-separators*
  (coerce (list #\Space #\Tab #\Newline #\Return #\Page
		#\. #\, #\; #\! #\? #\" #\' #\` #\|
		#\( #\) #\[ #\] #\{ #\}
		#\# #\@ #\% #\^ #\$ #\~ #\\)
	  'string)
  "Characters that separate tokens: whitespace and punctuation that is NOT
   part of a Lisp identifier. Chars like - * + = / < > : & are intentionally
   left out so symbol names (make-instance, string=, *features*, &rest,
   :initarg) survive as single tokens.")

(defun tokenize-text (text)
  "Lowercase TEXT and split it into a list of term strings, keeping Lisp
   identifier characters. Drops empty tokens; no stopword removal."
  (remove-if (lambda (s) (zerop (length s)))
	     (uiop:split-string (string-downcase text)
				:separator *token-separators*)))

(defparameter *stop-words*
  '("a" "an" "the" "is" "are" "was" "were" "be" "been" "being"
    "have" "has" "had" "do" "does" "did" "will" "would" "shall" "should"
    "may" "might" "must" "can" "could" "am" "it" "its"
    "in" "on" "at" "to" "for" "of" "with" "by" "from" "as"
    "and" "or" "but" "not" "no" "nor" "so" "yet"
    "this" "that" "these" "those" "what" "which" "who" "whom"
    "i" "me" "my" "we" "our" "you" "your" "he" "she" "they" "them"
    "how" "when" "where" "why" "if" "then" "than" "about")
  "Common English stop words to filter from queries.")

(defun keyword-text (text)
  "Lowercase TEXT and split it into a list of term strings, removing
   filler words. Drops empty tokens."
  (remove-if (lambda (s) (member s *stop-words*))
	     (tokenize-text text)))

(defun split-into-chunk-offsets (text &key (chunk-size *default-chunk-size*)
				           (overlap *chunk-overlap*))
  "Split TEXT into overlapping chunks of CHUNK-SIZE characters.
   Tries to break at sentence level when possible."
  (let ((chunks nil)
	(len (length text))
	(start 0))
    (loop while (< start len)
	  do (let* ((end (min (+ start chunk-size) len))
		    (break-pos
		      (if (>= end len)
			  end
			  (or (position #\. text :start (max start (- end 80))
						 :end end :from-end t)
			      (position #\Newline text :start (max start (- end 80))
						       :end end :from-end t)
			      end)))
		    (actual-end (if (< break-pos end)
				    (1+ break-pos)
				    end))
		    (chunk (make-chunk-with-offsets :text
						    (string-trim '(#\Space #\Newline #\Tab)
								 (subseq text start actual-end))
						    :start-offset start
						    :end-offset actual-end)))
	       (when (> (length (chunk-with-offsets-text chunk)) 0)
		 (push chunk chunks))
	       (if (>= actual-end len)
		   (setf start len)
		   (setf start (- actual-end overlap)))))
    (nreverse chunks)))

(defun chunk-url-text-to-corpus (corpus url text &key (chunk-size *default-chunk-size*)
			                      (overlap *chunk-overlap*))
  "Split TEXT it into chunks, compute embeddings,
   and add the chunks to CORPUS. Returns the number of chunks added."
  (let ((chunks (split-into-chunk-offsets text :chunk-size chunk-size :overlap overlap))
	(count 0))
    (dolist (chunk chunks)
      (let* ((chunk-text (chunk-with-offsets-text chunk))
	     (embedding (run-ollama-embedding chunk-text))
	     (tokens (tokenize-text chunk-text)))
	(push (make-document-chunk-slim :url url
					:start-offset
					(chunk-with-offsets-start-offset chunk)
					:end-offset
					(chunk-with-offsets-end-offset chunk)
					:embedding embedding
					:tokens tokens)
	      (corpus-chunks corpus))
	(incf count)))
    count))

(defun make-corpus-from-url (url)
  (let* ((url-text (fetch-url-text url))
	 (corpus (make-corpus :name url)))
    (chunk-url-text-to-corpus corpus url url-text)
    ;; Corpus vector = centroid of its chunk embeddings (no LLM summary).
    (setf (corpus-keywords corpus)
	  (keyword-text url-text))
    (setf (corpus-embedding corpus)
	  (vector-centroid
	   (remove nil (mapcar #'document-chunk-slim-embedding
			       (corpus-chunks corpus)))))
    (setf (corpus-bm25-chunk-index corpus)
	  (make-bm25-index (corpus-chunks corpus)
			   :key #'document-chunk-slim-tokens))
    corpus))

(defun get-full-chunk-from-slim (slim-chunk)
  "Get the full chunk from a slim chunk by fetching the relevant
   url text."
  (let* ((url (document-chunk-slim-url slim-chunk))
	 (start (document-chunk-slim-start-offset slim-chunk))
	 (end (document-chunk-slim-end-offset slim-chunk))
	 (url-text (fetch-url-text url))
	 (chunk-text (subseq url-text start end))
	 (full-chunk (make-document-chunk :url url
					  :text chunk-text)))
    full-chunk))

(defun full-chunk-index-from-corpora (corpora)
  "Creates the full BM25 index from corpora."
  (make-bm25-index
   (loop for corpus in corpora append (corpus-chunks corpus))
   :key #'document-chunk-slim-tokens))


;;;; Formatting Functions


(defun format-retrieved-chunks (slim-chunks)
  "Format retrieved SLIM-CHUNKS (document-chunk-slim in ranked order) into a
   text string for LLM context. Each chunk's text is hydrated on demand here
   - the only place hydration happens - so we fetch only what is shown."
  (with-output-to-string (out)
    (loop for slim in slim-chunks
          for i from 1
          for chunk = (get-full-chunk-from-slim slim)
          do (format out "~%--- Retrieved Passage ~A (source: ~A) ---~%~A~%"
                     i
                     (document-chunk-url chunk)
                     (document-chunk-text chunk)))))
