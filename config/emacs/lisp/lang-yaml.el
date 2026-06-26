;;; lang-yaml.el --- YAML configuration -*- lexical-binding: t; -*-

;; yaml-ts-mode is built-in (Emacs 29+); treesit-auto handles remapping.

(use-package yaml-ts-mode
  :ensure nil
  :hook (yaml-ts-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '((yaml-mode yaml-ts-mode) . ("yaml-language-server" "--stdio"))))

(provide 'lang-yaml)
;;; lang-yaml.el ends here
