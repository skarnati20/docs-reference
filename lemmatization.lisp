;;;; lemmatization.lisp
;;;;
;;;; Implements Porter2 algorithm

(in-package :docs-reference)


(defparameter vowels
  '("a" "e" "i" "o" "u" "y"))

(defparameter doubles
  '("bb" "dd" "ff" "gg" "mm"
    "nn" "pp" "rr" "tt"))

(defparameter li-endings
  '("c" "d" "e" "g" "h"
    "k" "m" "n" "r" "t"))

(defun remove-suffix (word suffix)
  "Removes SUFFIX from WORD if it exists."
  (let ((suffix-len (length suffix))
	(word-len (length word))
	(suffix-pos (search suffix word :from-end)))
    (if (= suffix-pos (- (- word-len suffix-len) 1))
	(subseq 0 
	word))))
	


;;;; Remove Possesives


(defparameter possesive-suffixes
  '("'s'" "'s" "'"))


;;;; Replace S Endings

;; (defparameter s-endings
;;   '(("sses" . "ss")
;;     ("ied" . 


 
     
