;;; dream-benchmark.el --- Repeatable Dream startup benchmarks. -*- lexical-binding: t; -*-

(require 'cl-lib)

(setq user-emacs-directory
      (file-name-as-directory
       (expand-file-name ".." (file-name-directory load-file-name))))
(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))

(require 'dream-paths)
(require 'dream-startup)

(defconst dream-benchmark-schema 3)
(defconst dream-benchmark-warmups 2)
(defconst dream-benchmark-samples 20)
(defconst dream-benchmark-regression-limit 1.10)
(defconst dream-benchmark-idle-slice-limit 0.05)
(defconst dream-benchmark-metrics
  '(:startup :first-input :first-file :first-prog :environment
    :idle-slice-max))
(defconst dream-benchmark-file
  (expand-file-name "startup.el" dream-benchmark-directory))

(defun dream-benchmark--child-form ()
  "Return a self-contained form for an otherwise clean child process."
  `(let ((user-emacs-directory ,user-emacs-directory)
         (elapsed
          (lambda (function)
            (let ((start (current-time)))
              (funcall function)
              (float-time (time-subtract (current-time) start)))))
         (item-name
          (lambda (item)
            (if (and (eq (car-safe item) :feature)
                     (symbolp (cadr item)))
                (cadr item)
              (prin1-to-string item))))
         startup first-input first-file first-prog environment slices
         idle-items)
     (setq startup
           (funcall elapsed
            (lambda ()
              (load (expand-file-name "early-init.el" user-emacs-directory) nil t)
              (load (expand-file-name "init.el" user-emacs-directory) nil t))))
     (setq first-input
           (funcall elapsed
            (lambda () (run-hooks 'dream-first-input-hook))))
     (setq first-file
           (funcall elapsed
            (lambda () (run-hooks 'dream-first-file-hook))))
     (setq first-prog
           (funcall elapsed
            (lambda ()
              (with-temp-buffer (emacs-lisp-mode)))))
     (setq environment
           (funcall elapsed
            (lambda () (dream-runtime-apply-environment-snapshot))))
     (dolist (item (copy-sequence once--incremental-code))
       (unless (and (eq (car item) :feature) (featurep (cadr item)))
         (let ((seconds
                (let ((gc-cons-threshold most-positive-fixnum))
                  (funcall elapsed (lambda () (once--run item))))))
           (push seconds slices)
           (push (cons (funcall item-name item) seconds)
                 idle-items))))
     (princ "DREAM_BENCHMARK=")
     (prin1 (list :startup startup
                  :first-input first-input
                  :first-file first-file
                  :first-prog first-prog
                  :environment environment
                  :idle-slice-max (if slices (apply #'max slices) 0.0)
                  :idle-items (nreverse idle-items)))))

(defun dream-benchmark--child-arguments ()
  "Return command-line arguments for a clean benchmark child."
  (list "-Q" "--batch" "--eval"
        (prin1-to-string (dream-benchmark--child-form))))

(defun dream-benchmark--sample ()
  "Run one isolated benchmark sample and return its metrics plist."
  (let ((buffer (generate-new-buffer " *dream-benchmark*"))
        (program (expand-file-name invocation-name invocation-directory)))
    (unwind-protect
        (let ((status
               (apply #'call-process program nil buffer nil
                      (dream-benchmark--child-arguments))))
          (unless (zerop status)
            (error "Benchmark subprocess failed (%s): %s"
                   status (with-current-buffer buffer (buffer-string))))
          (with-current-buffer buffer
            (goto-char (point-min))
            (unless (search-forward "DREAM_BENCHMARK=" nil t)
              (error "Benchmark marker missing: %s" (buffer-string)))
            (read (current-buffer))))
      (kill-buffer buffer))))

(defun dream-benchmark--percentile (values percentile)
  "Return PERCENTILE from numeric VALUES using nearest-rank indexing."
  (unless values
    (error "Cannot calculate a percentile from an empty sample"))
  (let* ((sorted (sort (copy-sequence values) #'<))
         (rank (max 1 (ceiling (* percentile (length sorted)))))
         (index (min (1- (length sorted)) (1- rank))))
    (nth index sorted)))

(defun dream-benchmark--summary (samples)
  "Return p50/p95 values for each metric in SAMPLES."
  (let ((idle-table (make-hash-table :test #'equal))
        result)
    (dolist (metric dream-benchmark-metrics)
      (let ((values (mapcar (lambda (sample) (plist-get sample metric)) samples)))
        (setq result
              (plist-put result metric
                         (list :p50 (dream-benchmark--percentile values 0.50)
                               :p95 (dream-benchmark--percentile values 0.95))))))
    (dolist (sample samples)
      (dolist (item (plist-get sample :idle-items))
        (push (cdr item) (gethash (car item) idle-table))))
    (let (idle-summary)
      (maphash
       (lambda (name values)
         (push (cons name
                     (list :p50 (dream-benchmark--percentile values 0.50)
                           :p95 (dream-benchmark--percentile values 0.95)))
               idle-summary))
       idle-table)
      (setq result
            (plist-put result :idle-items
                       (sort idle-summary
                             (lambda (left right)
                               (string-lessp (format "%s" (car left))
                                             (format "%s" (car right))))))))
    result))

(defun dream-benchmark--write-baseline (summary)
  "Write benchmark SUMMARY for the current Emacs build."
  (dream-benchmark--check-budget summary)
  (make-directory dream-benchmark-directory t)
  (with-temp-file dream-benchmark-file
    (prin1 (list :schema dream-benchmark-schema
                 :samples dream-benchmark-samples
                 :emacs-version emacs-version
                 :system-configuration system-configuration
                 :native-version (dream-startup-native-version)
                 :summary summary)
           (current-buffer))
    (insert "\n")))

(defun dream-benchmark--read-baseline ()
  "Read the benchmark baseline or signal a useful error."
  (unless (file-readable-p dream-benchmark-file)
    (error "Benchmark baseline missing; run `make benchmark-baseline'"))
  (with-temp-buffer
    (insert-file-contents dream-benchmark-file)
    (read (current-buffer))))

(defun dream-benchmark--check-budget (summary)
  "Signal when SUMMARY exceeds an absolute interaction budget."
  (let ((idle-p95 (plist-get (plist-get summary :idle-slice-max) :p95)))
    (when (> idle-p95 dream-benchmark-idle-slice-limit)
      (error "Idle slice p95 exceeds %.0fms: %.3fms"
             (* dream-benchmark-idle-slice-limit 1000)
             (* idle-p95 1000)))))

(defun dream-benchmark--check (summary baseline)
  "Compare SUMMARY against BASELINE and enforce absolute latency limits."
  (unless (and (eq (plist-get baseline :schema) dream-benchmark-schema)
               (eq (plist-get baseline :samples) dream-benchmark-samples)
               (equal (plist-get baseline :emacs-version) emacs-version)
               (equal (plist-get baseline :system-configuration)
                      system-configuration)
               (equal (plist-get baseline :native-version)
                      (dream-startup-native-version)))
    (error "Benchmark baseline schema, sample count, or runtime is incompatible"))
  (dream-benchmark--check-budget summary)
  (dolist (metric dream-benchmark-metrics)
    (dolist (percentile '(:p50 :p95))
      (let* ((current (plist-get (plist-get summary metric) percentile))
             (accepted (plist-get
                        (plist-get (plist-get baseline :summary) metric)
                        percentile)))
        (when (> current (* accepted dream-benchmark-regression-limit))
          (error "%s %s regressed: %.6fs versus %.6fs"
                 metric percentile current accepted))))))

(defun dream-benchmark-run (&optional establish-baseline)
  "Run Dream benchmarks; write a baseline when ESTABLISH-BASELINE is non-nil."
  (dotimes (_ dream-benchmark-warmups) (dream-benchmark--sample))
  (let* ((samples (cl-loop repeat dream-benchmark-samples
                           collect (dream-benchmark--sample)))
         (summary (dream-benchmark--summary samples)))
    (if establish-baseline
        (dream-benchmark--write-baseline summary)
      (dream-benchmark--check summary (dream-benchmark--read-baseline)))
    (princ (format "Dream benchmark%s: %S\n"
                   (if establish-baseline " baseline" "") summary))))

(provide 'dream-benchmark)
;;; dream-benchmark.el ends here.
