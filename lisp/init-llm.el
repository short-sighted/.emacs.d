;;; init-llm.el --- On-demand LLM configuration. -*- lexical-binding: t; -*-

(require 'dream-setup)

(setup gptel
  (:autoload-this)
  (:when-loaded
    (setopt gptel-model 'qwen3-coder:30b
            gptel-backend
            (gptel-make-ollama "Ollama"
              :host "localhost:11434"
              :stream t
              :models '(qwen3-coder:30b)))))

(provide 'init-llm)
;;; init-llm.el ends here.
