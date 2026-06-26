;;; config-checkers.el --- Syntax checking and spellcheck -*- lexical-binding: t; -*-

;;; =========================================================================
;;; FLYMAKE — built-in syntax checking (replaces flycheck)
;;; Eglot integrates with flymake automatically when both are active.
;;; =========================================================================

(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
         ("M-n"     . flymake-goto-next-error)
         ("M-p"     . flymake-goto-prev-error)
         ("C-c ! l" . flymake-show-buffer-diagnostics)
         ("C-c ! L" . flymake-show-project-diagnostics))
  :custom
  (flymake-no-changes-timeout 0.5)
  (flymake-fringe-indicator-position 'right-fringe)
  (flymake-wrap-around nil))

;;; =========================================================================
;;; CONSULT-FLYMAKE — used by casual-avy's custom menu
;;; =========================================================================

;; consult-flymake.el ships inside the consult package.
(use-package consult-flymake
  :ensure nil
  :after (consult flymake))

;;; =========================================================================
;;; ISPELL — hunspell with en_AU dictionary
;;; =========================================================================

(use-package ispell
  :ensure nil
  :custom
  (ispell-program-name "hunspell")
  (ispell-local-dictionary "en_AU")
  (ispell-local-dictionary-alist
   '(("en_AU" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_AU") nil utf-8)))
  :config
  (setenv "DICTIONARY" "en_AU"))

;;; =========================================================================
;;; FLYSPELL — spell checking (built-in)
;;; +everywhere: prog-mode gets flyspell-prog-mode (strings+comments only)
;;; =========================================================================

(use-package flyspell
  :ensure nil
  :hook
  ((text-mode . flyspell-mode)
   (prog-mode . flyspell-prog-mode))
  :custom
  (flyspell-issue-message-flag nil)
  (flyspell-issue-welcome-flag nil))

(provide 'config-checkers)
;;; config-checkers.el ends here
