;; dream-incremental-loading.el --- Initialize incremental loading configurations. -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Loading pacakges incrementally.
;;
;;; Code:

(cl-eval-when (compile)
  (require 'borg)
  (require 'info)
  (require 'once))

(advice-add 'once--run-incrementally :around (lambda (fn &rest args)
                                 (quiet! (apply fn args))))

(setq once-idle-timer (if (daemonp) 0 1.5)
      once-incremental-run-interval 1.5)
(setq once-setup-keyword-aliases
      '(:once-require-incrementally :iload))

(once-enable-incremental-loading)

(provide 'dream-incremental-loading)
;;; dream-incremental-loading.el ends here.
