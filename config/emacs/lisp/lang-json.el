;;; lang-json.el --- JSON configuration -*- lexical-binding: t; -*-

;; json-ts-mode is built-in (Emacs 29+); treesit-auto handles remapping.
(use-package json-ts-mode
  :ensure nil
  :hook (json-ts-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '((json-mode json-ts-mode)
                 . ("vscode-json-language-server" "--stdio"))))

(provide 'lang-json)
;;; lang-json.el ends here
