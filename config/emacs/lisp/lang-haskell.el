;;; lang-haskell.el --- Haskell configuration -*- lexical-binding: t; -*-

(use-package haskell-mode
  :hook
  ((haskell-mode . eglot-ensure)
   (haskell-mode . (lambda ()
                     (setq-local dash-docs-docsets '("Haskell")))))
  :config
  (add-to-list 'eglot-server-programs
               '((haskell-mode haskell-ts-mode)
                 . ("haskell-language-server-wrapper" "--lsp"))))

(provide 'lang-haskell)
;;; lang-haskell.el ends here
