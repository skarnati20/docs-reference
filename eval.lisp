;;;; eval.lisp

(in-package :docs-reference)


(defun calculate-score (ref-urls ret-urls)
  "Score one query. REF-URLS are the relevant page URLs; RET-URLS is the
   ranked list of retrieved chunk URLs (best first, may repeat a page).
   Returns a plist:
     :rank   1-based rank of the first relevant retrieved URL, or NIL
     :rr     reciprocal rank (1/rank, or 0.0 if none) -> feeds MRR
     :hit    T if any relevant page was retrieved (Hit@k)
     :recall fraction of REF-URLS retrieved anywhere in RET-URLS"
  (labels ((relevant-p (url)
             (some (lambda (ref) (uiop:string-prefix-p ref url)) ref-urls)))
    (let* ((rank (loop for url in ret-urls
                       for i from 1
                       when (relevant-p url) return i))
           (found (count-if (lambda (ref)
                              (some (lambda (url) (uiop:string-prefix-p ref url))
                                    ret-urls))
                            ref-urls)))
      (list :rank rank
            :rr (if rank (/ 1.0 rank) 0.0)
            :hit (and rank t)
            :recall (if ref-urls (float (/ found (length ref-urls))) 0.0)))))

(defun mean-metric (key results)
  "Mean of (getf r KEY) over RESULTS, treating a true :hit as 1."
  (if (null results)
      0.0
      (/ (reduce #'+ results
                 :key (lambda (r) (let ((v (getf r key)))
                                    (cond ((eq v t) 1.0) ((null v) 0.0) (t v)))))
         (length results))))

(defun metrics-alist (results)
  "Aggregate n / MRR / Recall / Hit-rate over RESULTS as an alist (-> JSON object)."
  (list (cons "n" (length results))
        (cons "mrr" (mean-metric :rr results))
        (cons "recall" (mean-metric :recall results))
        (cons "hitRate" (mean-metric :hit results))))

(defun report-data (corpus top-k results run-notes &optional config)
  "Build the report as a nested alist/vector that cl-json encodes straight
   to a JSON object (arrays are vectors so cl-json emits them as JSON arrays).
   CONFIG records the knobs the run used, so a report identifies itself."
  (list (cons "corpus" corpus)
        (cons "topK" top-k)
        (cons "notes" run-notes)
        (cons "config" config)
        (cons "overall" (metrics-alist results))
        (cons "byType"
              (loop for type in '("exact" "conceptual" "broad")
                    for subset = (remove-if-not
                                  (lambda (r) (string-equal (getf r :type) type))
                                  results)
                    when subset collect (cons type (metrics-alist subset))))
        (cons "queries"
              (coerce
               (mapcar (lambda (r)
                         (list (cons "query" (getf r :query))
                               (cons "type" (getf r :type))
                               (cons "rank" (getf r :rank))
                               (cons "rr" (getf r :rr))
                               (cons "hit" (and (getf r :hit) t))
                               (cons "recall" (getf r :recall))))
                       results)
               'vector))))

(defun format-results (results)
  "Print RESULTS (the per-query plists) as a readable summary: overall and
   per-type MRR / Recall / Hit-rate, then the queries that missed.
   Returns RESULTS."
  (flet ((row (label rs)
           (format t "  ~14@a ~4d ~9,3f ~9,3f ~9,3f~%"
                   label (length rs)
                   (mean-metric :rr rs)
                   (mean-metric :recall rs)
                   (mean-metric :hit rs))))
    (format t "~&~%=== Eval results (~d queries) ===~%" (length results))
    (format t "  ~14@a ~4@a ~9@a ~9@a ~9@a~%" "" "n" "MRR" "Recall" "Hit")
    (row "overall" results)
    (dolist (type '("exact" "conceptual" "broad"))
      (let ((subset (remove-if-not
                     (lambda (r) (string-equal (getf r :type) type))
                     results)))
        (when subset (row type subset))))
    (let ((misses (remove-if (lambda (r) (getf r :hit)) results)))
      (format t "~%Misses (~d of ~d):~%" (length misses) (length results))
      (dolist (r misses)
        (format t "  [~a] ~a~%" (getf r :type) (getf r :query))))))


;;;; Profiles


(defparameter *eval-profiles*
  '((:name "hybrid"          :methods (:dense :bm25))
    (:name "dense-only"      :methods (:dense))
    (:name "bm25-only"       :methods (:bm25))
    (:name "colbert-only"    :methods (:colbert))
    (:name "dense+colbert"   :methods (:dense :colbert))
    (:name "all-methods"     :methods :all)
    (:name "dense+rewrite"   :methods (:dense) :use-rewrite t)
    (:name "dense+hyde"      :methods (:dense) :use-hyde t)
    (:name "dense+rerank"    :methods (:dense) :rerank t :rerank-end 30)
    (:name "hybrid+rewrite"  :methods (:dense :bm25) :use-rewrite t)
    (:name "hybrid+hyde"     :methods (:dense :bm25) :use-hyde t)
    (:name "hybrid+rerank"   :methods (:dense :bm25) :rerank t :rerank-end 30))
  "Retrieval configurations to compare. Each is a :NAME plus the knobs
   RUN-EVAL passes to GATHER-CHUNKS-NO-AGENTS.")

(defun profile-knobs (profile)
  "PROFILE without its :NAME, as an argument list for RUN-EVAL."
  (loop for (key value) on profile by #'cddr
	unless (eq key :name)
	  append (list key value)))

(defun config-alist (top-k methods use-rewrite use-hyde rerank rerank-start rerank-end)
  "The knobs a run used, recorded in its report so runs are self-describing."
  (list (cons "topK" top-k)
	(cons "methods" (coerce (mapcar #'string-downcase
					(mapcar #'symbol-name
						(resolve-search-methods methods)))
				'vector))
	(cons "useRewrite" (and use-rewrite t))
	(cons "useHyde" (and use-hyde t))
	(cons "rerank" (and rerank t))
	(cons "rerankStart" rerank-start)
	(cons "rerankEnd" rerank-end)))


;;;; Running


(defun run-eval (corpora eval-file-path
		 &key (top-k 10)
		      (methods *default-search-methods*)
		      (use-rewrite nil)
		      (use-hyde nil)
		      (rerank nil)
		      (rerank-start 0)
		      (rerank-end 30)
		      (output-file nil)
		      (run-notes nil)
		      (quiet nil))
  "Evaluate how well GATHER-CHUNKS-NO-AGENTS ranks chunks for the queries in
   EVAL-FILE-PATH (a JSON file) under the given knobs. Writes a JSON report
   (config, overall and by-type MRR / Recall@k / Hit@k, plus per-query results)
   to OUTPUT-FILE when given. Returns the per-query result plists."
  (with-open-file (stream eval-file-path)
    (let* ((data (cl-json:decode-json stream))
	   (corpus (cdr (assoc :corpus data)))
	   (base-url (cdr (assoc :base-url data)))
	   (queries (cdr (assoc :queries data)))
	   (results nil))
      (dolist (query-pairs queries)
	(let* ((query (cdr (assoc :query query-pairs)))
	       (type (cdr (assoc :type query-pairs)))
	       (ref-pages (cdr (assoc :pages query-pairs)))
	       (ref-urls (mapcar (lambda (p) (uiop:strcat base-url p)) ref-pages))
	       (chunks (handler-case
			   (gather-chunks-no-agents corpora query
						    :top-k top-k
						    :methods methods
						    :use-rewrite use-rewrite
						    :use-hyde use-hyde
						    :rerank rerank
						    :rerank-start rerank-start
						    :rerank-end rerank-end)
			 (error (e)
			   (format t "  ! query failed (~a): ~a~%" query e)
			   nil)))
	       (ret-urls (mapcar #'document-chunk-slim-url chunks)))
	  (push (append (list :query query :type type)
			(calculate-score ref-urls ret-urls))
		results)))
      (setf results (nreverse results))
      (let ((json (cl-json:encode-json-to-string
		   (report-data corpus top-k results run-notes
				(config-alist top-k methods use-rewrite use-hyde
					      rerank rerank-start rerank-end)))))
	(when output-file
	  (with-open-file (out output-file :direction :output
					   :if-exists :supersede
					   :if-does-not-exist :create)
	    (write-line json out)))
	(unless quiet
	  (write-line json)
	  (format-results results)))
      results)))

(defun run-eval-profiles (corpora eval-file-path
			  &key (profiles *eval-profiles*)
			       (top-k 10)
			       (output-dir nil)
			       (rank-by :recall))
  "Run every profile in PROFILES over the same query set and print a comparison
   ordered best-first by RANK-BY (:recall, :rr or :hit). Returns a list of
   (:name NAME :results RESULTS) in that order."
  (let ((rows nil))
    (dolist (profile profiles)
      (let* ((name (getf profile :name))
	     (output-file (when output-dir
			    (merge-pathnames (format nil "results-~a.json" name)
					     (uiop:ensure-directory-pathname output-dir)))))
	(format t "~&running profile ~a ...~%" name)
	(push (list :name name
		    :results (apply #'run-eval corpora eval-file-path
				    :top-k top-k
				    :run-notes name
				    :output-file output-file
				    :quiet t
				    (profile-knobs profile)))
	      rows)))
    (let ((ranked (sort (nreverse rows) #'>
			:key (lambda (row) (mean-metric rank-by (getf row :results))))))
      (format-profile-comparison ranked top-k rank-by)
      ranked)))

(defun format-profile-comparison (ranked top-k rank-by)
  "Print RANKED profiles as a table, best-first, and name the winner."
  (format t "~&~%=== Profile comparison (top-k ~a, ranked by ~a) ===~%" top-k rank-by)
  (format t "  ~18@a ~9@a ~9@a ~9@a ~9@a ~9@a~%"
	  "profile" "MRR" "Recall" "Hit" "exact-R" "concep-R")
  (dolist (row ranked)
    (let* ((results (getf row :results))
	   (subset (lambda (type)
		     (remove-if-not (lambda (r) (string-equal (getf r :type) type))
				    results))))
      (format t "  ~18@a ~9,3f ~9,3f ~9,3f ~9,3f ~9,3f~%"
	      (getf row :name)
	      (mean-metric :rr results)
	      (mean-metric :recall results)
	      (mean-metric :hit results)
	      (mean-metric :recall (funcall subset "exact"))
	      (mean-metric :recall (funcall subset "conceptual")))))
  (when ranked
    (format t "~%best by ~a: ~a (~,3f)~%"
	    rank-by
	    (getf (first ranked) :name)
	    (mean-metric rank-by (getf (first ranked) :results)))))
