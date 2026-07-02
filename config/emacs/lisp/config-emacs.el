;;; config-emacs.el --- Built-in Emacs package configuration -*- lexical-binding: t; -*-

;;; =========================================================================
;;; EXEC-PATH-FROM-SHELL — inherit shell PATH in GUI/daemon Emacs
;;; =========================================================================

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;;; =========================================================================
;;; UNIQUIFY — unique buffer names showing path context
;;; =========================================================================

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'forward)  ; show parent dirs, not <2>
  (uniquify-separator "/")
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*"))

;;; =========================================================================
;;; DIRED
;;; =========================================================================

(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-alh --group-directories-first")
  (dired-dwim-target t)
  (dired-auto-revert-buffer t)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-create-destination-dirs 'ask)) ; prompt to create parent dirs on copy

(use-package dired-x
  :ensure nil
  :after dired
  :hook (dired-mode . dired-omit-mode)
  :custom
  ;; Also hide __pycache__, .DS_Store, .direnv noise.
  (dired-omit-files
   (rx (or (seq bol (? "#") (? ".") "#")  ; lock files
           (seq bol "..")                  ; parent dir
           "__pycache__"
           ".DS_Store"
           ".direnv"))))

;;; =========================================================================
;;; IBUFFER
;;; =========================================================================

(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :custom
  (ibuffer-expert t)
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-default-sorting-mode 'major-mode))

;;; =========================================================================
;;; SELECTION BEHAVIOUR (non-evil)
;;; =========================================================================

;; Type to replace the active region, like most editors expect.
(use-package delsel
  :ensure nil
  :config (delete-selection-mode 1))

;;; =========================================================================
;;; AUTO-REVERT — quiet, and also revert non-file buffers (dired etc.)
;;; =========================================================================

(use-package autorevert
  :ensure nil
  :custom
  (auto-revert-verbose nil)
  (global-auto-revert-non-file-buffers t))

;;; =========================================================================
;;; WINNER-MODE — undo/redo window configuration
;;; =========================================================================

(use-package winner
  :ensure nil
  :config (winner-mode 1))

;;; =========================================================================
;;; ELECTRIC
;;; =========================================================================

(use-package electric
  :ensure nil
  :config
  (electric-indent-mode 1))

;;; =========================================================================
;;; VC
;;; =========================================================================

(use-package vc-hooks
  :ensure nil
  :custom
  (vc-handled-backends '(Git)))

;;; =========================================================================
;;; ESHELL
;;; =========================================================================

(use-package eshell
  :ensure nil
  :custom
  (eshell-directory-name       (expand-file-name "eshell/" my/local-dir))
  (eshell-history-size 10000)
  (eshell-buffer-maximum-lines 10000)
  (eshell-scroll-to-bottom-on-input t))

(use-package em-smart
  :ensure nil
  :after eshell
  :custom (eshell-where-to-jump 'begin))

;;; =========================================================================
;;; ISEARCH
;;; =========================================================================

(use-package isearch
  :ensure nil
  :custom
  (isearch-lazy-count t)
  (lazy-count-prefix-format "(%s/%s) ")
  (isearch-allow-motion t)
  (isearch-wrap-pause 'no)
  ;; Fold diacritics by default: searching "cafe" matches "café".
  (search-default-mode #'char-fold-to-regexp))

;;; =========================================================================
;;; TRAMP — remote file state
;;; =========================================================================

(use-package tramp
  :ensure nil
  :custom
  (tramp-persistency-file-name (expand-file-name "tramp" my/local-dir)))

;;; =========================================================================
;;; TRANSIENT — persistent history/values/levels
;;; =========================================================================

(use-package transient
  :custom
  (transient-history-file (expand-file-name "transient/history.el" my/local-dir))
  (transient-values-file  (expand-file-name "transient/values.el"  my/local-dir))
  (transient-levels-file  (expand-file-name "transient/levels.el"  my/local-dir)))

;;; =========================================================================
;;; PROJECT.EL — replaces projectile
;;; =========================================================================

(use-package project
  :ensure nil
  :custom
  (project-list-file (expand-file-name "projects" my/local-dir))
  (project-switch-commands
   '((project-find-file    "Find file"  ?f)
     (project-find-regexp  "Find regexp" ?g)
     (consult-ripgrep      "Ripgrep"    ?r)
     (project-dired        "Dired"      ?d)
     (project-eshell       "Eshell"     ?e)
     (magit-project-status "Magit"      ?m))))

;;; =========================================================================
;;; XREF — remove etags backend, ripgrep for search
;;; =========================================================================

(use-package xref
  :ensure nil
  :custom
  (xref-search-program 'ripgrep)
  :config
  ;; etags is rarely useful and slows down xref — remove it; dumb-jump fills the gap.
  (remove-hook 'xref-backend-functions #'etags--xref-backend))

;;; =========================================================================
;;; PROG-MODE HOOKS — subword, URLs, comment continuation, prettify
;;; =========================================================================

(use-package prog-mode
  :ensure nil
  :hook
  (;; Navigate camelCase/snake_case sub-words with M-f/M-b.
   (prog-mode . subword-mode)
   ;; Make URLs in comments and strings clickable.
   (prog-mode . goto-address-prog-mode)
   (text-mode . goto-address-mode)
   ;; Prettify symbol sequences (-> ≠ == etc.)
   (prog-mode . prettify-symbols-mode))
  :config
  ;; RET inside a comment continues it with the comment prefix.
  (setq comment-multi-line t)
  (add-hook 'prog-mode-hook
            (lambda ()
              (setq-local comment-line-break-function
                          #'comment-indent-new-line))))

;;; =========================================================================
;;; WHICH-KEY — built-in Emacs 30+
;;; =========================================================================

(use-package which-key
  :ensure nil
  :custom
  (which-key-idle-delay 0.5)
  :config
  (which-key-mode 1))

;;; =========================================================================
;;; HELPFUL — better help buffers
;;; =========================================================================

(use-package helpful
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-h x" . helpful-command)
   ("C-h o" . helpful-symbol)))

;;; =========================================================================
;;; EDIFF — diff in the same frame, side by side
;;; =========================================================================

(use-package ediff
  :ensure nil
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally))

;;; =========================================================================
;;; PROCED — process manager with auto-refresh
;;; =========================================================================

(use-package proced
  :ensure nil
  :custom
  (proced-auto-update-flag t)
  (proced-auto-update-interval 5)
  (proced-enable-color-flag t))

;;; =========================================================================
;;; TABULATED-LIST — q quits in any tabulated buffer
;;; =========================================================================

(with-eval-after-load 'tabulated-list
  (keymap-set tabulated-list-mode-map "q" #'quit-window))

;;; =========================================================================
;;; BREADCRUMB — header-line function/class context
;;; =========================================================================

(use-package breadcrumb
  :hook (prog-mode . breadcrumb-local-mode))

(provide 'config-emacs)
;;; config-emacs.el ends here
