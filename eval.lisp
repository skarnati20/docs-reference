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

(defun report-data (corpus top-k results run-notes)
  "Build the report as a nested alist/vector that cl-json encodes straight
   to a JSON object (arrays are vectors so cl-json emits them as JSON arrays)."
  (list (cons "corpus" corpus)
        (cons "topK" top-k)
        (cons "notes" run-notes)
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

(defun filter-by-gap (ranked &key (max-gap 10.0) (min-keep 5))
  "Filters a ranked list to those items whose score is within MAX-GAP
   of RANKED's top score. Keeps a minimum of MIN-KEEP items."
  (let* ((best (reduce #'max ranked :key #'cdr))
	 (kept (remove-if (lambda (x) (< (cdr x) (- best max-gap))) ranked)))
    (if (< (length kept) min-keep)
        (subseq (sort (copy-list ranked) #'> :key #'cdr)
                0 (min min-keep (length ranked)))
        kept)))

(defun optimize-chunks (query chunks top-k mode &key (max-gap 10.0))
  "Produce the final ranked chunk list for one eval query.
   Three modes: (1) None, (2) Re-Rank, (3) Cross-Encoder Filter"
  (ecase mode
	(:re-rank (mapcar #'car
			   (rerank-chunks query
					  (subseq chunks 0 (min (* top-k 5) (length chunks)))
					  :top-k top-k)))
	(:filter (let* ((shortlist (subseq chunks 0 (min (* top-k 5) (length chunks))))
			(ranked-chunks (rank-chunks-in-order query shortlist))
			(kept (filter-by-gap ranked-chunks :max-gap max-gap)))
		   (mapcar #'car (subseq kept 0 (min top-k (length kept))))))
	(:none (if top-k
		   (subseq chunks 0 (min top-k (length chunks)))
		   chunks))))

(defun run-eval (corpora eval-file-path
                 &key (top-k 10) (output-file nil) (run-notes nil) (mode :none) (max-gap 10.0))
  "Evaluate how well SEARCH-CORPORA ranks chunks for the queries in
   EVAL-FILE-PATH (a JSON file). Writes a JSON report (overall + by-type
   MRR / Recall@k / Hit@k, plus per-query results) to stdout and, if given,
   OUTPUT-FILE. Returns the per-query result plists."
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
               (chunks (handler-case (search-corpora corpora query)
                         (error (e)
                           (format t "  ! query failed (~a): ~a~%" query e)
                           nil)))
               (final-chunks (optimize-chunks query chunks top-k mode
                                             :max-gap max-gap))
               (ret-urls (mapcar #'document-chunk-slim-url final-chunks)))
          (push (append (list :query query :type type)
                        (calculate-score ref-urls ret-urls))
                results)))
      (setf results (nreverse results))
      (let ((json (cl-json:encode-json-to-string
                   (report-data corpus top-k results run-notes))))
        (write-line json)
        (when output-file
          (with-open-file (out output-file :direction :output
                                           :if-exists :supersede
                                           :if-does-not-exist :create)
            (write-line json out)))
        (format-results results)))))
