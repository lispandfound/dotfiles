;;; config-ui.el --- Visual appearance and UI packages -*- lexical-binding: t; -*-

;;; =========================================================================
;;; THEME & FONT
;;; =========================================================================

(use-package emacs
  :ensure nil
  :config
  ;; modus-operandi is built-in since Emacs 28.
  (load-theme 'modus-operandi t)
  (set-face-attribute 'default nil
                      :family "JetBrainsMono Nerd Font"
                      :height 140
                      :weight 'regular
                      :width 'normal))

;;; =========================================================================
;;; ICONS
;;; =========================================================================

(use-package nerd-icons)

;;; =========================================================================
;;; MODELINE
;;; =========================================================================

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 28)
  (doom-modeline-bar-width 3)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (doom-modeline-buffer-encoding 'nondefault)
  (doom-modeline-check 'simple)
  (doom-modeline-minor-modes nil))

;;; =========================================================================
;;; HL-TODO — highlight TODO/FIXME/NOTE/HACK/REVIEW
;;; =========================================================================

(use-package hl-todo
  :hook ((prog-mode text-mode) . hl-todo-mode))

;;; =========================================================================
;;; LIGATURES — JetBrainsMono
;;; =========================================================================

(use-package ligature
  :config
  (ligature-set-ligatures
   'prog-mode
   '("->" "-->" "=>" "==>" "!=" "!==" "==" "===" "<=" ">=" "&&" "||"
     "++" "--" "::" ":::" ":=" "/*" "*/" "//" "///" "/**" "..." "<-"
     "<<" ">>" ">>>" "<<<" "<>" "/=" "~>" "<~" "<=>" ">->" "<-<"))
  (global-ligature-mode t))

;;; =========================================================================
;;; VC GUTTER — diff indicators in the fringe
;;; =========================================================================

(use-package diff-hl
  :hook
  ;; after-init triggers the load and activates the global modes.
  ((after-init         . global-diff-hl-mode)
   (after-init         . diff-hl-flydiff-mode)
   (magit-pre-refresh  . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh)
   (dired-mode         . diff-hl-dired-mode))
  :config
  ;; C-c v hunk navigation (mirrors Doom's <leader> v vc-gutter keys)
  (keymap-set my/vc-map "r" #'diff-hl-revert-hunk)
  (keymap-set my/vc-map "s" #'diff-hl-stage-current-hunk)
  (keymap-set my/vc-map "n" #'diff-hl-next-hunk)
  (keymap-set my/vc-map "p" #'diff-hl-previous-hunk))

;;; =========================================================================
;;; TAB-BAR — workspaces
;;; =========================================================================

(use-package tab-bar
  :ensure nil
  :custom
  (tab-bar-show 1)
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-tab-hints t)
  (tab-bar-close-button-show nil)
  (tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
  :config
  (tab-bar-mode 1)
  (tab-bar-history-mode 1))

;;; =========================================================================
;;; DASHBOARD — startup splash screen
;;; =========================================================================

(use-package dashboard
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-center-content t)
  (dashboard-projects-backend 'project-el)
  (dashboard-items '((recents   . 8)
                     (bookmarks . 5)
                     (projects  . 5)))
  :config
  (dashboard-setup-startup-hook))

;;; =========================================================================
;;; WINDOW DIVIDERS — thin lines between windows
;;; =========================================================================

(use-package emacs
  :ensure nil
  :config
  (setq window-divider-default-right-width 1
        window-divider-default-bottom-width 1
        window-divider-default-places 'right-only)
  (window-divider-mode 1))

;;; =========================================================================
;;; ADAPTIVE-WRAP — smart continuation-line indentation
;;; =========================================================================

(use-package adaptive-wrap
  :hook (visual-line-mode . adaptive-wrap-prefix-mode))

;; Enable soft wrapping in text modes (replaces :editor word-wrap).
(add-hook 'text-mode-hook #'visual-line-mode)

;;; =========================================================================
;;; PULSAR — flash current line after navigation jumps
;;; Replaces the manual pulse advice; integrates with avy, consult, xref.
;;; =========================================================================

(use-package pulsar
  :custom
  (pulsar-pulse t)
  (pulsar-delay 0.055)
  (pulsar-iterations 10)
  ;; Use the same face as pulse.el — picks up the active theme automatically.
  (pulsar-face 'pulse-highlight-start-face)
  (pulsar-highlight-face 'pulse-highlight-start-face)
  :config
  (pulsar-global-mode 1)
  (add-hook 'consult-after-jump-hook #'pulsar-recenter-top)
  (add-hook 'consult-after-jump-hook #'pulsar-reveal-entry)
  (dolist (fn '(avy-goto-char avy-goto-char-2 avy-goto-word-0
                avy-goto-line xref-find-definitions xref-go-back))
    (add-to-list 'pulsar-pulse-functions fn)))

;;; =========================================================================
;;; PULSE — flash region on yank / kill (built-in, complements pulsar)
;;; =========================================================================

(use-package pulse
  :ensure nil
  :config
  ;; Flash the yanked region after yank/yank-pop.
  (dolist (cmd '(yank yank-pop))
    (advice-add cmd :after
                (lambda (&rest _)
                  (when (and (mark t) (not (equal (mark t) (point))))
                    (pulse-momentary-highlight-region
                     (min (mark t) (point))
                     (max (mark t) (point)))))))
  ;; Flash the region before kill-ring-save removes it visually.
  (advice-add 'kill-ring-save :around
              (lambda (fn beg end &rest args)
                (pulse-momentary-highlight-region beg end)
                (apply fn beg end args))))

(provide 'config-ui)
;;; config-ui.el ends here
