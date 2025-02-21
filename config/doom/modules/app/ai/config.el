;;; tools/ai/config.el -*- lexical-binding: t; -*-


(use-package! aider
  :config
  ;; Set the model and API key
  (setq aider-args '("--model" "gemini/gemini-2.0-flash"))
  ;; Define a keybinding for the aider transient menu
  (map! :leader
        :desc "Assistant..."
        "-" #'aider-transient-menu))
