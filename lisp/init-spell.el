;;; init-spell.el --- Spell checking. -*- lexical-binding: t; -*-

(require 'dream-setup)

(cl-eval-when (compile)
  (require 'jinx))

(setup jinx
  (:set jinx-languages "en")
  (:hook-into text-mode prog-mode conf-mode))

(provide 'init-spell)
;;; init-spell.el ends here.
