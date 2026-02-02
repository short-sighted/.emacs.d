;;; init-llm.el --- Initialize LLM Configurations.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(setup gptel
  (:autoload-this)
  (:opt gpt-model 'qwen3-coder:30b
        gptel-backend (gptel-make-ollama "Ollama"
                        :host "localhost:11434"
                        :stream t
                        :models '(qwen3-coder:30b))))

(provide 'init-llm)
;;; init-llm.el ends here.
