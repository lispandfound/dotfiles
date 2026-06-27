;;; config-editor.el --- Editor enhancements -*- lexical-binding: t; -*-

;;; =========================================================================
;;; APHELEIA — async code formatting on save
;;; =========================================================================

(use-package apheleia
  :config
  (apheleia-global-mode 1))

;;; =========================================================================
;;; YASNIPPET — snippet expansion
;;; =========================================================================

(use-package yasnippet
  :custom
  (yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory)))
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets)

;;; =========================================================================
;;; AUTO-INSERT — file templates (built-in)
;;; =========================================================================

(use-package autoinsert
  :ensure nil
  :custom (auto-insert-query nil)
  :config (auto-insert-mode 1))

;;; =========================================================================
;;; HIDESHOW — code folding (built-in)
;;; =========================================================================

(use-package hideshow
  :ensure nil
  :hook (prog-mode . hs-minor-mode)
  :bind (:map hs-minor-mode-map
              ;; C-c h is the prefix; h/H/S/l hang off it.
              ("C-c h h" . hs-toggle-hiding)
              ("C-c h H" . hs-hide-all)
              ("C-c h S" . hs-show-all)
              ("C-c h l" . hs-hide-level)))

;;; =========================================================================
;;; EXPAND-REGION — grow/shrink selection by semantic units
;;; Bound globally to C-= in config-keys.el.
;;; =========================================================================

(use-package expand-region
  :commands (er/expand-region er/contract-region er/mark-symbol er/mark-word))

;;; =========================================================================
;;; DRAG-STUFF — move lines or regions up/down with M-up/M-down
;;; =========================================================================

(use-package drag-stuff
  :config
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))   ; sets up M-up / M-down / M-left / M-right

;;; =========================================================================
;;; LINK-HINT — avy-style link jumping in text / help / info buffers
;;; =========================================================================

(use-package link-hint
  :bind ("C-c o l" . link-hint-open-link)
  :config
  (with-eval-after-load 'help-mode
    (keymap-set help-mode-map "o" #'link-hint-open-link))
  (with-eval-after-load 'info
    (keymap-set Info-mode-map "o" #'link-hint-open-link))
  (with-eval-after-load 'apropos
    (keymap-set apropos-mode-map "o" #'link-hint-open-link)))

;;; =========================================================================
;;; VUNDO — visual undo tree
;;; =========================================================================

(use-package vundo
  :bind ("C-x u" . vundo)
  :custom (vundo-glyph-alist vundo-unicode-symbols))

;; Increase undo limits (Doom's :emacs undo module does this).
(use-package emacs
  :ensure nil
  :custom
  (undo-limit        (* 80  1024 1024))
  (undo-strong-limit (* 120 1024 1024))
  (undo-outer-limit  (* 360 1024 1024)))

;;; =========================================================================
;;; CASUAL SUITE — transient menus for built-in modes
;;; Full replication of editor/casual custom module.
;;;
;;; `casual' is the monorepo package; all casual-* sub-packages are provided
;;; by it and must use :ensure nil.  casual-avy is a separate package.
;;; =========================================================================

(use-package casual :ensure t)

(use-package casual-calc
  :ensure nil
  :after calc
  :bind (:map calc-mode-map ("C-o" . casual-calc-tmenu)))

(use-package casual-info
  :ensure nil
  :after info
  :bind (:map Info-mode-map ("C-o" . casual-info-tmenu)))

(use-package casual-dired
  :ensure nil
  :after (dired transient)
  :bind (:map dired-mode-map ("C-o" . casual-dired-tmenu)))

(use-package casual-avy
  :ensure t
  :bind ("M-g" . my/custom-avy-tmenu)
  :config
  (defun my/custom-avy-tmenu ()
    "Custom avy transient menu extended with consult integrations."
    (interactive)
    (require 'casual-avy)
    (transient-append-suffix 'casual-avy-tmenu "M-n"
      '("E" "Error" consult-compile-error :transient nil))
    (transient-append-suffix 'casual-avy-tmenu "E"
      '("f" "Flymake Error" consult-flymake))
    (transient-append-suffix 'casual-avy-tmenu "p"
      '("o" "Outline Item" consult-outline))
    (transient-append-suffix 'casual-avy-tmenu "o"
      '("i" "Imenu Item" consult-imenu))
    (transient-append-suffix 'casual-avy-tmenu "i"
      '("j" "Resume last jump" avy-resume))
    (casual-avy-tmenu)))

(use-package casual-make
  :ensure nil
  :after make-mode
  :bind (:map makefile-mode-map ("C-o" . casual-make-tmenu)))

(use-package casual-isearch
  :ensure nil
  :bind (:map isearch-mode-map ("C-o" . casual-isearch-tmenu)))

(use-package casual-ibuffer
  :ensure nil
  :after ibuffer
  :bind (:map ibuffer-mode-map
              ("C-o" . casual-ibuffer-tmenu)
              ("F"   . casual-ibuffer-filter-tmenu)
              ("s"   . casual-ibuffer-sortby-tmenu)
              ("{"   . ibuffer-backwards-next-marked)
              ("}"   . ibuffer-forward-next-marked)
              ("["   . ibuffer-backward-filter-group)
              ("]"   . ibuffer-forward-filter-group)
              ("$"   . ibuffer-toggle-filter-group)))

(use-package casual-image
  :ensure nil
  :bind (:map image-mode-map ("C-o" . casual-image-tmenu)))

(use-package casual-bookmarks
  :ensure nil
  :after bookmark
  :bind (:map bookmark-bmenu-mode-map
              ("C-o" . casual-bookmarks-tmenu)
              ("S"   . casual-bookmarks-sortby-tmenu)
              ("J"   . bookmark-jump)))

(use-package casual-agenda
  :ensure nil
  :after org-agenda
  :bind (:map org-agenda-mode-map
              ("C-o" . casual-agenda-tmenu)
              ("M-j" . org-agenda-clock-goto)
              ("J"   . bookmark-jump)))

(use-package casual-editkit
  :ensure nil
  :bind ("C-c C-h" . casual-editkit-main-tmenu)
  :hook (rectangle-mark-mode
         . (lambda ()
             (keymap-set rectangle-mark-mode-map "C-o"
                         #'casual-editkit-rectangle-tmenu))))

;;; =========================================================================
;;; ibuffer-project — group and filter ibuffer listing by project
;;; =========================================================================
(use-package ibuffer-project
  :hook (ibuffer-hook . (lambda ()
                          (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups)))))
(provide 'config-editor)
;;; config-editor.el ends here
