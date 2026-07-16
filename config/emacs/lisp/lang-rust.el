;;; lang-rust.el --- Rust configuration -*- lexical-binding: t; -*-

;; rust-ts-mode is built-in (Emacs 29+); treesit-auto handles remapping.

(use-package rust-mode
  :hook ((rust-mode    . eglot-ensure)
         (rust-ts-mode . eglot-ensure))
  :init (setq rust-mode-treesitter-derive t))

(provide 'lang-rust)
;;; lang-rust.el ends here
