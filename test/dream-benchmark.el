;;; dream-benchmark.el --- Repeatable Dream startup benchmarks. -*- lexical-binding: t; -*-

(require 'cl-lib)

(setq user-emacs-directory
      (file-name-as-directory
       (expand-file-name ".." (file-name-directory load-file-name))))
(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))

(require 'dream-paths)
(require 'dream-startup)

(defconst dream-benchmark-warmups 2)
(defconst dream-benchmark-samples 15)
(defconst dream-benchmark-regression-limit 1.10)
(defconst dream-benchmark-file
  (expand-file-name "startup.el" dream-benchmark-directory))

(defun dream-benchmark--elapsed (function)
  "Return wall-clock seconds used by FUNCTION."
  (let ((start (current-time)))
    (funcall function)
    (float-time (time-subtract (current-time) start))))

(defun dream-benchmark--child-form ()
  "Return the form evaluated in each isolated benchmark process."
  `(let ((user-emacs-directory ,user-emacs-directory)
         startup first-input first-prog slices)
     (setq startup
           (dream-benchmark--elapsed
            (lambda ()
              (load (expand-file-name "early-init.el" user-emacs-directory) nil t)
              (load (expand-file-name "init.el" user-emacs-directory) nil t))))
     (setq first-input
           (dream-benchmark--elapsed
            (lambda () (run-hooks 'on-first-input-hook))))
     (setq first-prog
           (dream-benchmark--elapsed
            (lambda ()
              (with-temp-buffer (emacs-lisp-mode)))))
     (dolist (item (copy-sequence once--incremental-code))
       (unless (and (eq (car item) :feature) (featurep (cadr item)))
         (push (dream-benchmark--elapsed
                (lambda () (once--run item)))
               slices)))
     (princ "DREAM_BENCHMARK=")
     (prin1 (list :startup startup
                  :first-input first-input
                  :first-prog first-prog
                  :idle-slice-max (if slices (apply #'max slices) 0.0)))))

(defun dream-benchmark--sample ()
  "Run one isolated benchmark sample and return its metrics plist."
  (let ((buffer (generate-new-buffer " *dream-benchmark*"))
        (program (expand-file-name invocation-name invocation-directory)))
    (unwind-protect
        (let ((status
               (call-process program nil buffer nil "-Q" "--batch"
                             "-L" (expand-file-name "test" user-emacs-directory)
                             "--load" (expand-file-name "test/dream-benchmark.el"
                                                        user-emacs-directory)
                             "--eval" (prin1-to-string
                                       (dream-benchmark--child-form)))))
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
  (let* ((sorted (sort (copy-sequence values) #'<))
         (index (floor (* percentile (1- (length sorted))))))
    (nth index sorted)))

(defun dream-benchmark--summary (samples)
  "Return p50/p95 values for each metric in SAMPLES."
  (let (result)
    (dolist (metric '(:startup :first-input :first-prog :idle-slice-max))
      (let ((values (mapcar (lambda (sample) (plist-get sample metric)) samples)))
        (setq result
              (plist-put result metric
                         (list :p50 (dream-benchmark--percentile values 0.50)
                               :p95 (dream-benchmark--percentile values 0.95))))))
    result))

(defun dream-benchmark--write-baseline (summary)
  "Write benchmark SUMMARY for the current Emacs build."
  (make-directory dream-benchmark-directory t)
  (with-temp-file dream-benchmark-file
    (prin1 (list :emacs-version emacs-version
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

(defun dream-benchmark--check (summary baseline)
  "Compare SUMMARY against BASELINE and signal on a 10%% regression."
  (unless (and (equal (plist-get baseline :emacs-version) emacs-version)
               (equal (plist-get baseline :native-version)
                      (dream-startup-native-version)))
    (error "Benchmark baseline belongs to a different Emacs/native ABI"))
  (dolist (metric '(:startup :first-input :first-prog :idle-slice-max))
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
