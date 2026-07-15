;;; lang-rust.el --- Rust configuration -*- lexical-binding: t; -*-

;; rust-ts-mode is built-in (Emacs 29+); treesit-auto handles remapping.

(defvar my/rust-error-regexp
  "\\s-*--> \\([^:\n]+\\):\\([0-9]+\\):\\([0-9]+\\)"
  "Regexp matching rustc/cargo file:line:col error locations.")

(with-eval-after-load 'compile
  (add-to-list 'compilation-error-regexp-alist-alist
               `(rustc ,my/rust-error-regexp 1 2 3))
  (add-to-list 'compilation-error-regexp-alist-alist
               `(cargo ,my/rust-error-regexp 1 2 3))
  (add-to-list 'compilation-error-regexp-alist 'cargo)
  (add-to-list 'compilation-error-regexp-alist 'rustc))

(defun my/rust--setup ()
  "Configure a Rust buffer for compilation."
  (setq-local compile-command "cargo build"))

(use-package rust-mode
  :hook
  ((rust-mode    . my/rust--setup)
   (rust-ts-mode . my/rust--setup)
   (rust-mode    . eglot-ensure)
   (rust-ts-mode . eglot-ensure)))

(provide 'lang-rust)
;;; lang-rust.el ends here
