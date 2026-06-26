;;; lang-elisp.el --- Emacs Lisp configuration -*- lexical-binding: t; -*-

;; flymake has a built-in elisp checker; no eglot needed.
(use-package elisp-mode
  :ensure nil
  :hook
  ((emacs-lisp-mode . flymake-mode)
   (emacs-lisp-mode . eldoc-mode))
  :bind (:map emacs-lisp-mode-map
         ("C-c C-e" . eval-last-sexp)
         ("C-c C-b" . eval-buffer)
         ("C-c C-z" . ielm)))

;; eros overlays are activated per-buffer in config-tools.el.

(provide 'lang-elisp)
;;; lang-elisp.el ends here
