;;; config-emacs.el --- Built-in Emacs package configuration -*- lexical-binding: t; -*-

;;; =========================================================================
;;; GCMH — automatic GC threshold tuning
;;; Replaces the manual gc-cons-threshold reset that used to live in
;;; early-init.el's emacs-startup-hook: gcmh keeps the threshold high during
;;; active use and only collects during idle time.
;;; =========================================================================

(use-package gcmh
  :hook (emacs-startup . gcmh-mode))

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
           ;; Any dotfiles.
           (seq bol "." (+ anychar))
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
  :config
  (winner-mode 1))

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

;;; --- Doom-style lambda prompt ---------------------------------------------

(defface my/eshell-prompt-pwd
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for the working directory in the eshell prompt.")

(defface my/eshell-prompt-git
  '((t :inherit font-lock-string-face))
  "Face for the git branch in the eshell prompt.")

(defface my/eshell-prompt-remote
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for the user@host indicator on remote eshell prompts.")

(defun my/eshell-git-branch ()
  "Return the current git branch by reading .git/HEAD, or nil.
Reads the ref file directly rather than shelling out, so it adds no
subprocess cost to every prompt."
  (when-let* ((root (locate-dominating-file default-directory ".git"))
              (head (expand-file-name ".git/HEAD" root))
              ((file-exists-p head)))
    (with-temp-buffer
      (insert-file-contents head)
      (goto-char (point-min))
      (if (looking-at "ref: refs/heads/\\(.*\\)")
          (match-string 1)
        ;; Detached HEAD — show the short SHA.
        (buffer-substring (point) (min (+ (point) 7) (point-max)))))))

(defun my/eshell-prompt ()
  "A Doom-inspired multiline eshell prompt.
Shows user@host when remote, the abbreviated working directory, the git
branch (local only, to keep remote prompts snappy), and a lambda that is
green on success and red on a non-zero exit status."
  (let ((status eshell-last-command-status))
    (concat
     "\n"
     (when (file-remote-p default-directory)
       (propertize
        (concat (or (file-remote-p default-directory 'user) "")
                "@" (or (file-remote-p default-directory 'host) "") " ")
        'face 'my/eshell-prompt-remote))
     (propertize (abbreviate-file-name (eshell/pwd)) 'face 'my/eshell-prompt-pwd)
     (when-let* (((not (file-remote-p default-directory)))
                 (branch (my/eshell-git-branch)))
       (propertize (concat "  " branch) 'face 'my/eshell-prompt-git))
     "\n"
     (propertize "λ" 'face (if (zerop status) 'success 'error))
     " ")))

(use-package eshell
  :ensure nil
  :custom
  (eshell-directory-name       (expand-file-name "eshell/" my/local-dir))
  (eshell-history-size 10000)
  (eshell-buffer-maximum-lines 10000)
  (eshell-scroll-to-bottom-on-input t)
  (eshell-prompt-function #'my/eshell-prompt)
  (eshell-prompt-regexp "^λ ")
  ;; We colour the prompt ourselves, so don't let eshell re-face it.
  (eshell-highlight-prompt nil)
  (eshell-history-append t)
  (eshell-hist-ignoredups t))

(use-package em-smart
  :ensure nil
  :after eshell
  :custom (eshell-where-to-jump 'begin))

;;; --- Fish-like syntax highlighting ----------------------------------------

(use-package eshell-syntax-highlighting
  :after eshell
  :config (eshell-syntax-highlighting-global-mode 1))

;;; =========================================================================
;;; EAT — terminal emulator + eshell interactive-command backend
;;; A pure-Elisp terminal (no native compilation, unlike vterm).  As a
;;; standalone terminal it runs full-screen/interactive apps well; via
;;; `eat-eshell-mode' it routes eshell command output through a real
;;; terminal emulator so htop, vim, ssh, colours and progress bars all
;;; behave — while everything else about eshell stays the same.
;;; =========================================================================

(use-package eat
  ;; Recipe comes from elpaca's menu (MELPA/NonGNU), which bundles the
  ;; required terminfo files.
  ;;
  ;; Enable via `eshell-first-time-mode-hook' rather than `eshell-load-hook':
  ;; the latter fires once when eshell loads, which under elpaca's async
  ;; activation can happen before this package is set up (so the hook is empty
  ;; and the mode never turns on).  The first-time-mode hook fires when you
  ;; actually open eshell — always after elpaca is done — and eat's enable
  ;; routine retro-fits the current buffer, so interactive commands work
  ;; immediately.
  :hook (eshell-first-time-mode . eat-eshell-mode)
  :custom
  (eat-kill-buffer-on-exit t)
  (eat-term-scrollback-size 200000))

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
  (tramp-persistency-file-name (expand-file-name "tramp" my/local-dir))
  :config
  ;; Adopt the remote login shell's own $PATH so eshell/commands can find
  ;; remote executables (fixes "nothing in my PATH" over TRAMP).  The literal
  ;; entries stay as fallbacks for hosts that don't export a useful PATH.
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;;; =========================================================================
;;; TRANSIENT — persistent history/values/levels
;;; =========================================================================

(use-package transient
  :ensure nil ;; NOTE: transient ensured separately early in init.el
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
;;; PROCED — process manager with transient UI, tree view, and optimizations
;;; =========================================================================

(defun my/casual-proced-checkbox (var label)
  (format "[%s] %s" (if var "X" " ") label))

(transient-define-prefix my/casual-proced-sort-tmenu ()
  "Sort the Proced process listing."
  ["Sort by"
   ("p" "PID"    proced-sort-pid    :transient t)
   ("c" "CPU%"   proced-sort-pcpu   :transient t)
   ("m" "Mem%"   proced-sort-pmem   :transient t)
   ("t" "Time"   proced-sort-time   :transient t)
   ("u" "User"   proced-sort-user   :transient t)
   ("s" "Start"  proced-sort-start  :transient t)]
  [:class transient-row
   ("C-g" "quit" transient-quit-one)
   ("q" "Quit"   transient-quit-seq)])

(transient-define-prefix my/casual-proced-tmenu ()
  "Transient menu for Proced process manager."
  :refresh-suffixes t

  [:inapt-if-not-derived 'proced-mode
   ["Signal & Priority"
    ("k" "Kill (SIGTERM)"  (lambda () (interactive) (proced-send-signal 15)) :transient t)
    ("K" "Kill (SIGKILL)"  (lambda () (interactive) (proced-send-signal 9))  :transient t)
    ("i" "Interrupt (SIGINT)" (lambda () (interactive) (proced-send-signal 2)) :transient t)
    ("s" "Signal…" (lambda () (interactive) (proced-send-signal)) :transient t)
    ("R" "Renice…" proced-renice :transient t)]

   ["Filter"
    ("f" "Filter…" proced-filter-interactive
     :description (lambda () (format "Filter: %s" proced-filter))
     :transient t)
    ("F" "Format…" proced-format-interactive
     :description (lambda () (format "Format: %s" proced-format))
     :transient t)
    ("o" "Omit Process" proced-omit-processes :transient t)
    ("y" "User (mine)"  (lambda () (interactive) (proced-filter 'user)) :transient t)
    ("a" "All"          (lambda () (interactive) (proced-filter 'all)) :transient t)
    ("r" "Running"      (lambda () (interactive) (proced-filter 'all-running)) :transient t)
    ("e" "Emacs"        (lambda () (interactive) (proced-filter 'emacs)) :transient t)]

   ["Tree"
    ("T" "Toggle Tree" proced-toggle-tree
     :description (lambda () (my/casual-proced-checkbox proced-tree-flag "Tree"))
     :transient t)
    ("C" "Mark Children"   proced-mark-children :transient t)
    ("P" "Mark Parents"    proced-mark-parents :transient t)]]

  [:inapt-if-not-derived 'proced-mode
   ["Sort & Mark"
    ("S" "Sort by›" my/casual-proced-sort-tmenu :transient t)
    ("m" "Mark"          proced-mark          :transient t)
    ("u" "Unmark"        proced-unmark        :transient t)
    ("U" "Unmark All"    proced-unmark-all    :transient t)
    ("t" "Toggle Marks"  proced-toggle-marks   :transient t)]

   ["Actions"
    ("g" "Refresh" revert-buffer :transient t)
    ("A" "Auto-Upd" proced-toggle-auto-update
     :description (lambda () (my/casual-proced-checkbox proced-auto-update-flag "Auto-Upd"))
     :transient t)]

   ["Help"
    ("?" "Proced Help" proced-help)]]

  [:class transient-row
   ("C-g" "quit" transient-quit-one)
   ("q" "Quit Proced" quit-window)])

(use-package proced
  :ensure nil
  :custom
  (proced-auto-update-flag t)
  (proced-auto-update-interval 10)
  (proced-enable-color-flag t)
  (proced-show-remote-processes nil)
  (proced-tree-flag t)
  (proced-tree-depth 3)
  (proced-format 'short)
  (proced-filter 'user)
  (proced-sort 'pcpu)
  (proced-descend t)
  :bind (:map proced-mode-map
              ("C-o" . my/casual-proced-tmenu)))

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

(use-package bookmark
  :ensure nil
  :custom (bookmark-default-file (expand-file-name "bookmarks" my/local-dir)))

(use-package fancy-compilation
  :after compile
  :custom
  (fancy-compilation-override-colors nil)
  (fancy-compilation-term "xterm-256color")
  :config (fancy-compilation-mode))

(provide 'config-emacs)
;;; config-emacs.el ends here
