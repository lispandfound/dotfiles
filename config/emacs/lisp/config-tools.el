;;; config-tools.el --- Tools and integrations -*- lexical-binding: t; -*-

;;; =========================================================================
;;; TRAMP — remote file optimisations (verbatim from config.el §4)
;;; =========================================================================

(use-package tramp
  :ensure nil
  :custom
  (remote-file-name-inhibit-locks t)
  (tramp-use-scp-direct-remote-copying t)
  (remote-file-name-inhibit-auto-save-visited t)
  (tramp-copy-size-limit (* 1024 1024))
  (tramp-verbose 2)
  (tramp-default-method "rsync")
  (tramp-use-connection-share nil)
  (tramp-persistency-file-name (expand-file-name "tramp" my/local-dir))
  :config
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))
  (connection-local-set-profiles
   '(:application tramp :protocol "scp") 'remote-direct-async-process)
  (connection-local-set-profiles
   '(:application tramp :protocol "ssh") 'remote-direct-async-process)
  (connection-local-set-profiles
   '(:application tramp :protocol "rsync") 'remote-direct-async-process)
  ;; compile adds a hook that disables SSH ControlMaster; remove it.
  (with-eval-after-load 'compile
    (remove-hook 'compilation-mode-hook
                 #'tramp-compile-disable-ssh-controlmaster-options)))

;; Shared remote memoization helper (verbatim from config.el).
(defun memoize-remote (key cache orig-fn &rest args)
  "Memoize a value when KEY is a remote path."
  (if (and key (file-remote-p key))
      (if-let ((current (assoc key (symbol-value cache))))
          (cdr current)
        (let ((current (apply orig-fn args)))
          (set cache (cons (cons key current) (symbol-value cache)))
          current))
    (apply orig-fn args)))

(defcustom my/eglot-remote-never-modes '(sh-mode bash-ts-mode)
  "Major modes that never start eglot on remote files, without prompting."
  :type '(repeat symbol)
  :group 'eglot)

(defvar my/eglot-remote--session-always nil
  "Modes approved for eglot on remote files for this Emacs session.")

(defvar my/eglot-remote--session-never nil
  "Modes blocked from eglot on remote files for this Emacs session.")

(defun my/eglot-ensure-unless-remote ()
  "Start eglot, prompting before starting on a remote file.

On remote files, modes in `my/eglot-remote-never-modes' are silently
skipped.  Session-level decisions are honoured.  Otherwise prompts:
  y   start eglot for this file
  n   skip eglot for this file
  a   always start eglot for this mode this session
  N   never start eglot for this mode this session"
  (if (not (file-remote-p (or buffer-file-name default-directory)))
      (eglot-ensure)
    (let ((mode major-mode))
      (cond
       ((memq mode my/eglot-remote-never-modes) nil)
       ((memq mode my/eglot-remote--session-never) nil)
       ((memq mode my/eglot-remote--session-always) (eglot-ensure))
       (t
        (pcase (read-char-choice
                (format "Start eglot on remote %s? [y]es [n]o [a]lways [N]ever-for-type: "
                        mode)
                '(?y ?n ?a ?N))
          (?y (eglot-ensure))
          (?n nil)
          (?a (push mode my/eglot-remote--session-always) (eglot-ensure))
          (?N (push mode my/eglot-remote--session-never) nil)))))))

;; Memoize project-current for remote paths to avoid repeated TRAMP lookups.
(defvar my/project-current-cache nil)
(defun my/memoize-project-current (orig &optional prompt directory)
  (memoize-remote (or directory project-current-directory-override default-directory)
                  'my/project-current-cache orig prompt directory))
(advice-add 'project-current :around #'my/memoize-project-current)

;; Strip heavy features on remote buffers (adapted from config.el §4).
(defun my/tramp-optimization-hook ()
  "Disable expensive features when visiting remote files."
  (when (file-remote-p default-directory)
    (setq-local vc-handled-backends nil)
    (display-line-numbers-mode -1)
    (when (fboundp 'diff-hl-mode) (diff-hl-mode -1))
    (when (fboundp 'flymake-mode) (flymake-mode -1))
    (when (fboundp 'flyspell-mode) (flyspell-mode -1))
    (eldoc-mode -1)
    (when (boundp 'doom-modeline-buffer-file-name-style)
      (setq-local doom-modeline-buffer-file-name-style 'file-name))))

(add-hook 'find-file-hook  #'my/tramp-optimization-hook)
(add-hook 'dired-mode-hook #'my/tramp-optimization-hook)

;;; =========================================================================
;;; EGLOT — LSP client (built-in Emacs 29+)
;;; Individual server registrations live in each lang-*.el file.
;;; =========================================================================

(use-package eglot
  :ensure nil
  :custom
  (eglot-autoshutdown t)
  (eglot-confirm-server-initiated-edits nil)
  (eglot-events-buffer-size 0)
  (eglot-sync-connect 1)
  :config
  ;; Ignore .zarr directories from file-watching (replaces lsp-file-watch-ignored-directories).
  ;; eglot-ignored-directories was added in eglot 1.17; guard for older builds.
  (when (boundp 'eglot-ignored-directories)
    (add-to-list 'eglot-ignored-directories "\\.zarr")))

;;; =========================================================================
;;; TREESIT-AUTO — automatic tree-sitter grammar management
;;; Handles +tree-sitter across all lang modules.
;;; =========================================================================

(use-package treesit
  :ensure nil
  :custom
  (treesit-extra-load-path (list (expand-file-name "tree-sitter/" my/local-dir)))
  :config
  (setq treesit--install-language-grammar-out-dir-history
        (list (expand-file-name "tree-sitter/" my/local-dir))))

(use-package treesit-auto
  :custom (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;; =========================================================================
;;; AVY — character-based jump navigation
;;; =========================================================================

(use-package avy
  :custom
  (avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s))
  (avy-all-windows nil)       ; jump in current window by default
  (avy-all-windows-alt t)     ; C-u prefix jumps across all windows
  (avy-background t)          ; dim background during avy jump
  (avy-single-candidate-jump nil) ; always show jump target, even for one match
  :config
  (defun avy-zap-up-to-char ()
    "Prompt for a char, zap up to (but not including) the selected instance."
    (interactive)
    (let ((start (point)))
      (avy-with avy-zap-up-to-char
        (call-interactively #'avy-goto-char)
        (kill-region start (point)))))
  (keymap-global-set "<remap> <zap-to-char>" #'avy-zap-up-to-char))

;;; =========================================================================
;;; MAGIT + FORGE
;;; =========================================================================

(use-package magit
  :custom
  (magit-tramp-pipe-stty-settings 'pty)
  (magit-process-find-password-functions '(magit-process-password-auth-source))
  :bind (:map my/vc-map
              ("/" . magit-dispatch)
              ("." . magit-file-dispatch)
              ("g" . magit-status)
              ("G" . magit-status-here)
              ("B" . magit-blame-addition)
              ("C" . magit-clone)
              ("F" . magit-fetch)
              ("L" . magit-log-buffer-file)
              ("S" . magit-stage-file)
              ("U" . magit-unstage-file)
              ("R" . vc-revert))
  :config
  ;; Memoize magit-toplevel for remote directories.
  (defvar magit-toplevel-cache nil)
  (defun memoize-magit-toplevel (orig &optional directory)
    (memoize-remote (or directory default-directory)
                    'magit-toplevel-cache orig directory))
  (advice-add 'magit-toplevel :around #'memoize-magit-toplevel))

(use-package forge
  :after magit
  :bind (:map my/vc-map
              ("'" . forge-dispatch)))

(use-package magit-lfs :after magit)

;;; =========================================================================
;;; GIT-LINK (C-c v l)
;;; =========================================================================

(use-package git-link
  :bind (:map my/vc-map
              ("l" . git-link)))

;;; =========================================================================
;;; ENVRC — direnv integration (replaces :tools direnv)
;;; =========================================================================

(use-package envrc
  :hook (after-init . envrc-global-mode))

;;; =========================================================================
;;; DOCKER
;;; =========================================================================

(use-package docker)

;;; =========================================================================
;;; GPTEL — LLM integration (verbatim from config.el §5)
;;; =========================================================================

(use-package gptel
  :config
  (setq-default gptel-backend
                (gptel-make-ollama "Ollama-Cloud"
                  :host "localhost:11434"
                  :stream t
                  :models '(deepseek-v4-flash:cloud))
                gptel-model 'deepseek-v4-flash:cloud)
  (gptel-make-gh-copilot "Copilot"))

;;; =========================================================================
;;; IGIST — GitHub Gists (SPC n g → C-c n g)
;;; =========================================================================

(use-package igist
  :bind (:map my/notes-map
              ("g" . igist-dispatch)))


;;; =========================================================================
;;; DWIM-SHELL-COMMAND
;;; =========================================================================

(use-package dwim-shell-command
  :bind (([remap shell-command] . dwim-shell-command)
         :map dired-mode-map
         ([remap dired-do-async-shell-command] . dwim-shell-command)
         ([remap dired-do-shell-command]       . dwim-shell-command)
         ([remap dired-smart-shell-command]    . dwim-shell-command)))

;; dwim-shell-commands.el ships inside the dwim-shell-command package.
(use-package dwim-shell-commands
  :ensure nil
  :after dwim-shell-command)

;;; =========================================================================
;;; EDIT-SERVER — edit browser text areas in Emacs
;;; =========================================================================

(use-package edit-server
  :commands edit-server-start
  :init
  (if after-init-time
      (edit-server-start)
    (add-hook 'after-init-hook #'edit-server-start))
  :custom
  (edit-server-new-frame-alist
   '((name . "Edit with Emacs FRAME")
     (top . 200) (left . 200)
     (width . 80) (height . 25)
     (minibuffer . t) (menu-bar-lines . t))))

;;; =========================================================================
;;; DAPE — debugger (replaces dap-mode; works without lsp-mode)
;;; =========================================================================

(use-package dape
  :hook
  ((kill-emacs . dape-breakpoint-save)
   (after-init . dape-breakpoint-load))
  :custom
  (dape-default-breakpoints-file (expand-file-name "dape-breakpoints" my/local-dir))
  :config
  (add-hook 'dape-on-stopped-hooks #'dape-info)
  (add-hook 'dape-on-stopped-hooks #'dape-repl))

;;; =========================================================================
;;; EROS — eval overlays for :tools eval +overlay
;;; =========================================================================

(use-package eros
  :hook (emacs-lisp-mode . eros-mode))

;;; =========================================================================
;;; PDF-TOOLS
;;; =========================================================================

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config (pdf-tools-install :no-query))

;;; =========================================================================
;;; LOOKUP — xref + dumb-jump + dash-docs + dictionary
;;; =========================================================================

(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package dash-docs
  :custom
  (dash-docs-browser-func #'eww)
  (dash-docs-enable-debugging nil))

(use-package consult-dash
  :after (consult dash-docs)
  :bind (:map my/lookup-map
              ("d" . consult-dash)))

(use-package dictionary
  :ensure nil
  :custom (dictionary-server "dict.org")
  :bind (:map my/lookup-map
              ("w" . dictionary-search)))

;; Hoogle search — replaces (+lookup-provider-url-alist '("Hoogle" ...))
(defun my/hoogle-search (query)
  "Search Hoogle for QUERY."
  (interactive "sHoogle: ")
  (browse-url (format "https://hoogle.mangoiv.com/?q=%s" (url-encode-url query))))
(keymap-set my/lookup-map "h" #'my/hoogle-search)

;;; =========================================================================
;;; DENOTE — note-taking ecosystem (from tools/denote custom module)
;;; =========================================================================

(use-package denote
  :hook
  ((after-init . denote-rename-buffer-mode)
   (dired-mode . denote-dired-mode))
  :custom
  (denote-directory (expand-file-name "~/notes/"))
  :bind (:map my/denote-map
              ("d" . denote)
              ("j" . denote-journal-new-or-existing-entry)
              ("n" . denote-open-or-create)
              ("t" . denote-date)
              ("+" . denote-subdirectory)
              :map my/denote-bk-map
              ("r" . denote-rename-file)
              ("f" . denote-rename-file-using-front-matter)
              ("k" . denote-rename-file-keywords)
              :map my/denote-link-map
              ("i" . denote-link)
              ("h" . denote-org-link-to-heading)
              ("b" . denote-backlinks)
              ("g" . denote-find-backlink)
              ("o" . denote-org-dblock-insert-backlinks)
              :map my/notes-map
              ("d" . denote)
              ("j" . denote-journal-new-or-existing-entry)
              ("n" . denote-open-or-create)))

(use-package denote-journal
  :after denote
  :custom
  (denote-journal-keyword "labnotes")
  (denote-journal-directory (expand-file-name "~/notes/labnotes"))
  (denote-journal-title-format 'day-date-month-year))

(use-package denote-review
  :after denote)

(use-package denote-org
  :after denote
  :custom
  (denote-org-store-link-to-heading 'id))

(use-package consult-notes
  :commands (consult-notes consult-notes-search-in-all-notes)
  :bind (:map my/denote-search-map
              ("n" . consult-notes)
              ("s" . consult-notes-search-in-all-notes)
              :map my/notes-map
              ("f" . consult-notes)
              ("s" . consult-notes-search-in-all-notes))
  :config
  (consult-notes-org-headings-mode)
  (consult-notes-denote-mode)
  (setopt consult-notes-denote-files-function
          (lambda () (denote-directory-files nil t t))))

;;; =========================================================================
;;; CYLC-MODE
;;; =========================================================================

;; cylc-mode lives inside the cylc-flow repo; use :ensure to pass the recipe.
(use-package cylc-mode
  :ensure (:host github :repo "cylc/cylc-flow"
                 :files ("cylc/flow/etc/syntax/cylc-mode.el"))
  :mode ("\\.cylc\\'" "suite.*\\.rc\\'"))

;;; =========================================================================
;;; CMAKE-MODE
;;; =========================================================================

(use-package cmake-mode)

;;; =========================================================================
;;; OX-SLACK — Org export to Slack format
;;; =========================================================================

(use-package ox-slack
  :after org)


;;; =========================================================================
;;; DIRED-RSYNC — Rsync transfer in dired
;;; =========================================================================

(use-package dired-rsync-transient
  :bind (:map dired-mode-map
              ("C-c C-r" . dired-rsync-transient)))

(provide 'config-tools)
;;; config-tools.el ends here
