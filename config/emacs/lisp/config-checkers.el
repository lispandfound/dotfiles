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
;;; JINX — fast spell checking via enchant/hunspell
;;; Checks strings and comments in prog-mode (respects syntax tables).
;;; =========================================================================

(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$"   . jinx-correct)
         ("C-M-$" . jinx-correct-all))
  :custom
  (jinx-languages "en_AU"))

;;; =========================================================================
;;; CONSULT-JINX — consult-powered correction picker
;;; Not on MELPA; installed directly from Codeberg.
;;; =========================================================================

(use-package consult-jinx
  :ensure (:host codeberg :repo "bingshan/emacs-consult-jinx")
  :after (jinx consult)
  :bind ([remap jinx-correct] . consult-jinx))

(provide 'config-checkers)
;;; config-checkers.el ends here
