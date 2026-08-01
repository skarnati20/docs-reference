;;;; lemmatization.lisp
;;;;
;;;; Implements Porter2 algorithm
;;;; NOTE: Implementation entirely from Claude

(in-package :docs-reference)


(defparameter vowels
  '("a" "e" "i" "o" "u" "y"))

(defparameter doubles
  '("bb" "dd" "ff" "gg" "mm"
    "nn" "pp" "rr" "tt"))

(defparameter li-endings
  '("c" "d" "e" "g" "h"
    "k" "m" "n" "r" "t"))

(defun vowel-p (char)
  "True if CHAR is a vowel. An uppercase Y, marked in the prelude, is a consonant."
  (and (member (string char) vowels :test #'string=) t))

(defun valid-li-ending-p (char)
  "True if CHAR may precede a removable -li."
  (and (member (string char) li-endings :test #'string=) t))

(defun ends-with-p (word suffix)
  "True if WORD ends in SUFFIX."
  (let ((word-len (length word))
	(suffix-len (length suffix)))
    (and (>= word-len suffix-len)
	 (string= suffix word :start2 (- word-len suffix-len)))))

(defun ends-with-double-p (word)
  "True if WORD ends in one of the DOUBLES."
  (and (some (lambda (double) (ends-with-p word double)) doubles) t))

(defun remove-suffix (word suffix)
  "Removes SUFFIX from WORD if it exists."
  (if (ends-with-p word suffix)
      (subseq word 0 (- (length word) (length suffix)))
      word))

(defun replace-suffix (word suffix replacement)
  "Replaces SUFFIX at the end of WORD with REPLACEMENT."
  (concatenate 'string (remove-suffix word suffix) replacement))

(defun longest-suffix (word suffixes)
  "The longest of SUFFIXES that WORD ends with, or NIL."
  (let ((best nil))
    (dolist (suffix suffixes best)
      (when (and (ends-with-p word suffix)
		 (> (length suffix) (length (or best ""))))
	(setf best suffix)))))

(defun suffix-start (word suffix)
  "Index in WORD at which SUFFIX begins."
  (- (length word) (length suffix)))


;;;; Regions


(defun region-start (word start)
  "Index after the first non-vowel following a vowel at or after START,
   or the length of WORD when there is no such non-vowel."
  (let ((len (length word)))
    (loop for i from start below (1- len)
	  when (and (vowel-p (char word i))
		    (not (vowel-p (char word (1+ i)))))
	    return (+ i 2)
	  finally (return len))))

(defparameter r1-exceptions
  '("gener" "commun" "arsen" "past" "univers"
    "later" "emerg" "organ" "inter")
  "Prefixes whose R1 is the rest of the word, to stop over-stemming
   (generate/general, universe/university, past/paste).")

(defun word-r1 (word)
  "Start index of R1."
  (let ((exception (find-if (lambda (prefix) (uiop:string-prefix-p prefix word))
			    r1-exceptions)))
    (if exception
	(length exception)
	(region-start word 0))))

(defun word-r2 (word r1)
  "Start index of R2."
  (region-start word r1))

(defun short-syllable-p (word)
  "True if WORD ends in a short syllable: a vowel between two non-vowels where
   the last is not w, x or Y; a two letter vowel/non-vowel word; or past."
  (let ((len (length word)))
    (cond
      ((ends-with-p word "past") t)
      ((>= len 3)
       (let ((before (char word (- len 3)))
	     (middle (char word (- len 2)))
	     (final (char word (1- len))))
	 (and (not (vowel-p before))
	      (vowel-p middle)
	      (not (vowel-p final))
	      (not (find final "wxY")))))
      ((= len 2)
       (and (vowel-p (char word 0))
	    (not (vowel-p (char word 1)))))
      (t nil))))

(defun short-word-p (word r1)
  "True if WORD ends in a short syllable and its R1 is null."
  (and (>= r1 (length word))
       (short-syllable-p word)))


;;;; Prelude and Postlude


(defun prelude (word)
  "Drops a leading apostrophe and marks every consonantal y as Y."
  (let ((result (copy-seq (if (uiop:string-prefix-p "'" word)
			      (subseq word 1)
			      word))))
    (when (plusp (length result))
      (when (char= (char result 0) #\y)
	(setf (char result 0) #\Y))
      (loop for i from 1 below (length result)
	    when (and (char= (char result i) #\y)
		      (vowel-p (char result (1- i))))
	      do (setf (char result i) #\Y)))
    result))

(defun postlude (word)
  "Turns the marked Y letters back into lower case."
  (substitute #\y #\Y word))


;;;; Remove Possesives


(defparameter possesive-suffixes
  '("'s'" "'s" "'"))

(defun step-0 (word)
  "Removes a possessive ending."
  (let ((suffix (longest-suffix word possesive-suffixes)))
    (if suffix
	(remove-suffix word suffix)
	word)))


;;;; Replace S Endings


(defun step-1a (word)
  "sses -> ss, ied/ies -> i or ie, and a plural s when a vowel precedes it."
  (let ((suffix (longest-suffix word '("sses" "ied" "ies" "us" "ss" "s"))))
    (cond
      ((null suffix) word)
      ((string= suffix "sses") (replace-suffix word "sses" "ss"))
      ((or (string= suffix "ied") (string= suffix "ies"))
       (replace-suffix word suffix
		       (if (> (suffix-start word suffix) 1) "i" "ie")))
      ((string= suffix "s")
       ;; Drop the s only when a vowel appears before the letter preceding it,
       ;; so gas and this keep it but gaps and kiwis lose it.
       (if (find-if #'vowel-p word :end (max 0 (- (length word) 2)))
	   (remove-suffix word "s")
	   word))
      (t word))))


;;;; Remove Verb Endings


(defparameter eed-exceptions
  '("proc" "exc" "succ")
  "Stems that keep -eed, since proceed and succeed are not past participles.")

(defparameter ing-exceptions
  '("inn" "out" "cann" "herr" "earr" "even")
  "Stems that keep -ing, so inning and herring do not become in and her.")

(defun respell-stem (word r1)
  "Fixes up a stem after -ed or -ing removal: restore a dropped e, undouble a
   final consonant, or add e back to a short word."
  (cond
    ((or (ends-with-p word "at")
	 (ends-with-p word "bl")
	 (ends-with-p word "iz"))
     (concatenate 'string word "e"))
    ((ends-with-double-p word)
     ;; hopp -> hop, but add, egg and off are left alone.
     (if (and (= (length word) 3) (find (char word 0) "aeo"))
	 word
	 (subseq word 0 (1- (length word)))))
    ((short-word-p word r1) (concatenate 'string word "e"))
    (t word)))

(defun step-1b (word r1)
  "eed/eedly -> ee in R1, and removes ed/edly/ing/ingly."
  (let ((suffix (longest-suffix word '("eed" "eedly" "ed" "edly" "ing" "ingly"))))
    (cond
      ((null suffix) word)
      ((or (string= suffix "eed") (string= suffix "eedly"))
       (let ((stem (remove-suffix word suffix)))
	 (if (and (>= (suffix-start word suffix) r1)
		  (not (member stem eed-exceptions :test #'string=)))
	     (concatenate 'string stem "ee")
	     word)))
      (t
       (let ((stem (remove-suffix word suffix)))
	 (cond
	   ;; dying -> die: exactly one non-vowel plus y before -ing.
	   ((and (string= suffix "ing")
		 (= (length stem) 2)
		 (char= (char stem 1) #\y)
		 (not (vowel-p (char stem 0))))
	    (concatenate 'string (subseq stem 0 1) "ie"))
	   ((and (string= suffix "ing")
		 (member stem ing-exceptions :test #'string=))
	    word)
	   ((find-if #'vowel-p stem) (respell-stem stem r1))
	   (t word)))))))

(defun step-1c (word)
  "Replaces a final y after a non-vowel with i, so cry -> cri but by is left."
  (let ((len (length word)))
    (if (and (>= len 3)
	     (find (char word (1- len)) "yY")
	     (not (vowel-p (char word (- len 2)))))
	(concatenate 'string (subseq word 0 (1- len)) "i")
	word)))


;;;; Replace Derivational Endings


(defparameter step-2-suffixes
  '(("tional" . "tion") ("enci" . "ence") ("anci" . "ance") ("abli" . "able")
    ("entli" . "ent") ("izer" . "ize") ("ization" . "ize") ("ational" . "ate")
    ("ation" . "ate") ("ator" . "ate") ("alism" . "al") ("aliti" . "al")
    ("alli" . "al") ("fulness" . "ful") ("ousli" . "ous") ("ousness" . "ous")
    ("iveness" . "ive") ("iviti" . "ive") ("biliti" . "ble") ("bli" . "ble")
    ("ogist" . "og") ("fulli" . "ful") ("lessli" . "less")))

(defun step-2 (word r1)
  "Replaces the longest derivational ending found in R1."
  (let ((suffix (longest-suffix word (append (mapcar #'car step-2-suffixes)
					     '("ogi" "li")))))
    (if (or (null suffix) (< (suffix-start word suffix) r1))
	word
	(let ((replacement (cdr (assoc suffix step-2-suffixes :test #'string=))))
	  (cond
	    (replacement (replace-suffix word suffix replacement))
	    ((string= suffix "ogi")
	     (if (ends-with-p (remove-suffix word "ogi") "l")
		 (replace-suffix word "ogi" "og")
		 word))
	    ((string= suffix "li")
	     (let ((stem (remove-suffix word "li")))
	       (if (and (plusp (length stem))
			(valid-li-ending-p (char stem (1- (length stem)))))
		   stem
		   word)))
	    (t word))))))

(defparameter step-3-suffixes
  '(("tional" . "tion") ("ational" . "ate") ("alize" . "al")
    ("icate" . "ic") ("iciti" . "ic") ("ical" . "ic")
    ("ful" . "") ("ness" . "")))

(defun step-3 (word r1 r2)
  "Replaces a second round of derivational endings found in R1."
  (let ((suffix (longest-suffix word (cons "ative"
					   (mapcar #'car step-3-suffixes)))))
    (cond
      ((or (null suffix) (< (suffix-start word suffix) r1)) word)
      ((string= suffix "ative")
       (if (>= (suffix-start word suffix) r2)
	   (remove-suffix word "ative")
	   word))
      (t (replace-suffix word suffix
			 (cdr (assoc suffix step-3-suffixes :test #'string=)))))))

(defparameter step-4-suffixes
  '("al" "ance" "ence" "er" "ic" "able" "ible" "ant" "ement"
    "ment" "ent" "ism" "ate" "iti" "ous" "ive" "ize"))

(defun step-4 (word r2)
  "Removes the longest residual ending found in R2."
  (let ((suffix (longest-suffix word (cons "ion" step-4-suffixes))))
    (cond
      ((or (null suffix) (< (suffix-start word suffix) r2)) word)
      ((string= suffix "ion")
       (let ((stem (remove-suffix word "ion")))
	 (if (and (plusp (length stem))
		  (find (char stem (1- (length stem))) "st"))
	     stem
	     word)))
      (t (remove-suffix word suffix)))))

(defun step-5 (word r1 r2)
  "Removes a final e, and undoubles a final ll."
  (let ((len (length word)))
    (cond
      ((and (plusp len) (char= (char word (1- len)) #\e))
       (let ((stem (subseq word 0 (1- len))))
	 (if (or (>= (1- len) r2)
		 (and (>= (1- len) r1) (not (short-syllable-p stem))))
	     stem
	     word)))
      ((and (>= len 2)
	    (char= (char word (1- len)) #\l)
	    (char= (char word (- len 2)) #\l)
	    (>= (1- len) r2))
       (subseq word 0 (1- len)))
      (t word))))


;;;; Stemming


(defparameter word-exceptions
  '(("skis" . "ski") ("skies" . "sky")
    ("idly" . "idl") ("gently" . "gentl") ("ugly" . "ugli")
    ("early" . "earli") ("only" . "onli") ("singly" . "singl")
    ("sky" . "sky") ("news" . "news") ("howe" . "howe")
    ("atlas" . "atlas") ("cosmos" . "cosmos") ("bias" . "bias")
    ("andes" . "andes"))
  "Words stemmed by lookup, plus the invariant forms that map to themselves.")

(defun stem-word (word)
  "Reduces WORD to its Porter2 stem."
  (let* ((word (string-downcase word))
	 (exception (assoc word word-exceptions :test #'string=)))
    (cond
      (exception (cdr exception))
      ((<= (length word) 2) word)
      (t (let* ((marked (prelude word))
		(r1 (word-r1 marked))
		(r2 (word-r2 marked r1))
		(stem (step-0 marked))
		(stem (step-1a stem))
		(stem (step-1b stem r1))
		(stem (step-1c stem))
		(stem (step-2 stem r1))
		(stem (step-3 stem r1 r2))
		(stem (step-4 stem r2))
		(stem (step-5 stem r1 r2)))
	   (postlude stem))))))

(defun stem-tokens (tokens)
  "Reduces each of TOKENS to its Porter2 stem."
  (mapcar #'stem-word tokens))
