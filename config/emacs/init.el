;;; -*- lexical-binding: t -*-
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))


(use-package transient
  :demand t)

(use-package modus-themes
  :demand t
  :config
  (load-theme 'modus-operandi t))

(use-package mood-line
  :demand t
  :config
  (require 'mood-line-segment-vc)
  (mood-line-mode))

(display-time-mode)
(keyboard-translate ?\C-t ?\C-x)
(keyboard-translate ?\C-x ?\C-t)
(add-hook 'server-after-make-frame-hook
          (lambda ()
            (keyboard-translate ?\C-t ?\C-x)
            (keyboard-translate ?\C-x ?\C-t)))
(global-unset-key (kbd "C-t"))          ; unbind the transpose-char key because it annoys me

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ("M-g i" . consult-imenu)
         ("M-g f" . consult-flymake)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-fd)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element
  :custom (consult-imenu-config
           '((emacs-lisp-mode :toplevel "Functions" :types
                              ((102 "Functions" font-lock-function-name-face)
                               (109 "Macros" font-lock-function-name-face)
                               (112 "Packages" font-lock-constant-face)
                               (116 "Types" font-lock-type-face)
                               (118 "Variables" font-lock-variable-name-face)))
             (python-ts-mode
              :types
              ((?f "Function" font-lock-function-name-face)
               (?m "Method" font-lock-function-name-face)
               (?c "Class" font-lock-property-use-face)
               (?M "Module" font-lock-builtin-face)
               (?F "Field" font-lock-regexp-face)
               (?v "Variable" font-lock-constant-face)))))
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  )

(use-package better-defaults
  :demand t
  :config
  (ido-mode -1))

(add-hook 'after-init-hook #'electric-pair-mode)
(add-hook 'after-init-hook #'electric-indent-mode)

(use-package corfu
  ;; Optional customizations
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :bind (:map corfu-map ("SPC" . corfu-insert-separator))
  :init
  (global-corfu-mode))


(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))


;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

(use-package vertico-directory
  :ensure nil
  :after vertico
  :init
  (keymap-set vertico-map "RET" #'vertico-directory-enter)
  (keymap-set vertico-map "DEL" #'vertico-directory-delete-char)
  (keymap-set vertico-map "M-DEL" #'vertico-directory-delete-word)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))


;; A few more useful configurations...
(use-package emacs
  :ensure nil
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (bind-key "M-o" #'other-window-prefix)
  ;; Support opening new minibuffers from inside existing minibuffers.
  (setq enable-recursive-minibuffers t)

  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.
  (setq read-extended-command-predicate #'command-completion-default-include-p))


;; A few more useful configurations...
(use-package emacs
  :ensure nil
  :init
  ;; TAB cycle if there are only few candidates
  ;; (setq completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)
  (setq tab-first-completion nil)


  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (setq text-mode-ispell-word-completion nil)

  ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
  ;; mode.  Corfu commands are hidden, since they are not used via M-x. This
  ;; setting is useful beyond Corfu.
  (setq read-extended-command-predicate #'command-completion-default-include-p))


(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("M-p p" . completion-at-point) ;; capf
         ("M-p t" . complete-tag)        ;; etags
         ("M-p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("M-p h" . cape-history)
         ("M-p f" . cape-file)
         ("M-p k" . cape-keyword)
         ("M-p s" . cape-elisp-symbol)
         ("M-p e" . cape-elisp-block)
         ("M-p a" . cape-abbrev)
         ("M-p l" . cape-line)
         ("M-p w" . cape-dict)
         ("M-p :" . cape-emoji)
         ("M-p \\" . cape-tex)
         ("M-p _" . cape-tex)
         ("M-p ^" . cape-tex)
         ("M-p &" . cape-sgml)
         ("M-p r" . cape-rfc1345))
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-hook 'completion-at-point-functions #'cape-history)
  ;;(add-hook 'completion-at-point-functions #'cape-keyword)
  ;;(add-hook 'completion-at-point-functions #'cape-tex)
  ;;(add-hook 'completion-at-point-functions #'cape-sgml)
  ;;(add-hook 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-hook 'completion-at-point-functions #'cape-abbrev)
  ;;(add-hook 'completion-at-point-functions #'cape-dict)
  ;;(add-hook 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-hook 'completion-at-point-functions #'cape-line)
  )

(use-package embark

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  (with-eval-after-load 'em-term
    (add-to-list 'eshell-visual-commands "watch"))
  :config
  (bind-key "E" #'eshell embark-file-map)

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package magit
  :bind ("C-c g g" . magit-status)
  :init (with-eval-after-load 'project
          (add-to-list 'project-switch-commands '(magit-project-status "Magit" "m"))))

(use-package forge
  :bind ("C-c '" . forge-dispatch))

(use-package git-link
  :bind (("C-c g l" . git-link)))

(use-package pyvenv)

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :demand t
  :config
  (global-treesit-auto-mode)
  (treesit-auto-add-to-auto-mode-alist 'all))

(use-package python
  :ensure nil
  :bind (:map python-ts-mode-map
              (:repeat-map python-indent-shift-right-repeat-map
                           (">" . python-indent-shift-right)
                           ("<" . python-indent-shift-left)))
  :init
  (setq major-mode-remap-alist
        '((python-mode . python-ts-mode)))

  :config


  (add-hook 'python-ts-mode-hook (lambda ()
                                   (setq-local transpose-sexps-function #'treesit-transpose-sexps
                                               python-shell-interpreter-args "--simple-prompt --classic"
                                               devdocs-current-docs '("pandas~2" "numpy~2.0" "python~3.13" "matplotlib")))))

(use-package cython-mode)


(use-package python-numpydoc
  :ensure nil
  :after (python)
  :load-path "lisp/"
  :bind (:map python-ts-mode-map
              ("M-n" . python-numpydoc-insert)))

(use-package comint-mime
  :hook ((shell-mode . comint-mime-setup)
         (inferior-python-mode . comint-mime-setup)))

(global-set-key [remap newline] #'newline-and-indent)

;; Configure Tempel
(use-package tempel
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))

  :init

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf t)
  (add-hook 'prog-mode-hook 'tempel-setup-capf t)
  (add-hook 'text-mode-hook 'tempel-setup-capf t)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
  )

(use-package tempel-collection
  :after tempel)

(use-package ws-butler
  :hook (prog-mode markdown-mode org-mode))

(setq-default abbrev-mode t)


(use-package eglot
  :ensure nil
  :custom
  (eglot-report-progress nil)
  :hook ((nix-ts-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (haskell-mode . eglot-ensure)
         (rust-mode . eglot-ensure))
  :bind (("C-c c r" . eglot-rename)
         ("C-c c e" . eglot)
         ("C-c c i" . eglot-code-action-organize-imports)
         ("C-c c a" . eglot-code-actions)
         ("C-c c q" . eglot-code-quickfix))
  :config
  (defun group-by-breadcrumb-kind (breadcrumb-list)
    "Group all values in BREADCRUMB-LIST by their breadcrumb-kind property.
Returns an alist where keys are breadcrumb-kind strings and values are lists
of corresponding breadcrumb entries."
    (let ((groups '()))
      (dolist (item breadcrumb-list)
        (let* ((breadcrumb-obj (car item))
               (name (substring-no-properties breadcrumb-obj))
               (position (car (get-text-property 0 'breadcrumb-region breadcrumb-obj)))
               (kind (get-text-property 0 'breadcrumb-kind breadcrumb-obj))
               (simple-item (cons name (set-marker (make-marker) position))))
          (when kind
            (let ((existing-group (assoc kind groups)))
              (if existing-group
                  (setcdr existing-group (cons simple-item (cdr existing-group)))
                (push (list kind simple-item) groups))))))
      ;; Reverse the order of items in each group to maintain original order
      (mapcar (lambda (group)
                (cons (car group) (reverse (cdr group))))
              (reverse groups))))
  (advice-add 'eglot-imenu :filter-return #'group-by-breadcrumb-kind)
  (setq-default eglot-workspace-configuration '(:basedpyright (:typeCheckingMode "standard")))
  (add-to-list 'eglot-server-programs '(nix-ts-mode . ("nil")))

  (defun my-filter-eglot-diagnostics (diags)
    "Drop Pyright 'variable not accessed' notes from DIAGS."
    (list (seq-remove (lambda (d)
                        (and (eq (flymake-diagnostic-type d) 'eglot-note)
                             (s-starts-with? "Pyright:" (flymake-diagnostic-text d))
                             (s-ends-with? "is not accessed" (flymake-diagnostic-text d))))
                      (car diags))))

  (advice-add 'eglot--report-to-flymake :filter-args #'my-filter-eglot-diagnostics))

(use-package eglot-booster
  :ensure (:host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config	(eglot-booster-mode))
(use-package flymake-ruff
  :ensure t
  :hook (eglot-managed-mode . flymake-ruff-load))

(use-package project
  :ensure nil
  :bind (("C-x p t" . project-test) ("C-x p i" . consult-imenu-multi))
  :init
  (defcustom project-test-command "just test" "Default test command for `project-test'")
  (defun project-test ()
    (interactive)
    (let ((compile-command project-test-command))
      (call-interactively #'project-compile))))

(use-package which-key
  :demand t
  :config
  (which-key-mode))

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

(setq dired-dwim-target t
      dired-auto-revert-buffer t
      dired-listing-switches "-alFh"
      isearch-lazy-count t
      tramp-use-ssh-controlmaster-options nil
      vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)" vc-ignore-dir-regexp tramp-file-name-regexp)
      use-short-answers t)

(use-package yaml-mode)

(use-package csv-mode
  :hook (csv-mode . csv-align-mode))


(use-package haskell-mode
  :bind (:map haskell-mode-map
              (("C-c C-c" . haskell-compile)))
  :init

  (add-to-list 'display-buffer-alist '("\\*haskell-compilation\\*.*" (display-buffer-in-side-window (side . bottom)))))

(add-to-list 'display-buffer-alist '("\\`.*e?shell\\*" (display-buffer-in-side-window (side . bottom))))

(use-package exec-path-from-shell
  :demand t
  :config
  (exec-path-from-shell-initialize))

(use-package dumb-jump
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(delete-selection-mode)

(use-package comment-dwim-2
  :bind ("M-;" . #'comment-dwim-2))

(use-package crux
  :bind (("C-k" . crux-smart-kill-line)
         ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
         ("C-c S" . crux-find-user-init-file)
         ("<M-return>" . crux-smart-open-line)
         ("C-^" . crux-top-join-line)
         ("<M-S-return>" . crux-smart-open-line-above)))

(use-package titlecase
  :bind (("C-c M-c" . titlecase-dwim)))


(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  :init
  (defun eshell--previous-directories ()
    (delete-dups (mapcar 'abbreviate-file-name
                         (ring-elements eshell-last-dir-ring))))
  (defun eshell/z (&optional regexp)
    "Navigate to a previously visited directory in eshell."
    (cond
     ((and (not regexp) (featurep 'consult-dir))
      (eshell/cd (substring-no-properties (consult-dir--pick "Switch directory: "))))
     (t (eshell/cd (if regexp (eshell-find-previous-directory regexp)
                     (completing-read "cd: " (eshell--previous-directories)))))))

  (defvar consult-dir--source-eshell `(:name "Eshell"
                                             :narrow ?e
                                             :category file
                                             :face consult-file
                                             :enabled ,(lambda () (and (boundp 'eshell-last-dir-ring) eshell-last-dir-ring))
                                             :items ,#'eshell--previous-directories))
  (defun consult-dir--zoxide-dirs ()
    "Return list of zoxide dirs."
    (mapcar (lambda (line) (concat line "/")) (split-string (shell-command-to-string "zoxide query --list") "\n" t)))

  ;; A consult source that calls this function
  (defvar consult-dir--source-zoxide
    `(:name     "Zoxide dirs"
                :narrow   ?z
                :category file
                :face     consult-file
                :history  file-name-history
                :enabled  ,(lambda () (executable-find "zoxide"))
                :items    ,#'consult-dir--zoxide-dirs)
    "Zoxide directory source for `consult-dir'.")

  ;; Adding to the list of consult-dir sources

  :config
  (add-to-list 'consult-dir-sources 'consult-dir--source-eshell t)
  (add-to-list 'consult-dir-sources 'consult-dir--source-zoxide t))

(use-package elm-mode)

(use-package wgrep)

(use-package org
  :ensure nil
  :bind (("C-c a" . 'org-agenda-transient)
         ("C-c x" . 'org-capture-transient))
  :custom
  (org-agenda-files '("~/org/todo.org"))
  (org-default-notes-file "~/org/notes.org")
  (org-directory "~/org")
  (org-todo-keywords '((sequence "TODO" "WAIT(w@/!)" "|" "DONE" "KILL")))
  :init
  (setq org-agenda-capture-file "~/org/todo.org")
  (defun org-setup-transient-interface ()
    (transient-define-prefix org-agenda-transient ()
      "Replace the org-agenda buffer by a transient."
      [["Built-in agendas"
        ("a" "Current day/week"
         (lambda () (interactive) (org-agenda nil "a")))
        ("t" "Global todo list"
         (lambda () (interactive) (org-agenda nil "t")))
        ("T" "Global todo list + choose"
         (lambda () (interactive) (org-agenda nil "T")))
        ("m" "Search tags" (lambda () (interactive) (org-agenda nil "m")))
        ("M" "Search tags with TODO"
         (lambda () (interactive) (org-agenda nil "M")))
        ("e" "Export" (lambda () (interactive) (org-agenda nil "e")))
        ("s" "Search" (lambda () (interactive) (org-agenda nil "s")))
        ("S" "Search with TODO"
         (lambda () (interactive) (org-agenda nil "S")))
        ("/" "Multi-occur" (lambda () (interactive) (org-agenda nil "/")))
        ("<" "Restrict" (lambda () (interactive) (org-agenda nil "<")))
        (">" "Remove restiction"
         (lambda () (interactive) (org-agenda nil ">")))
        ("#" "List stuck projects"
         (lambda () (interactive) (org-agenda nil "#")))
        ("!" "Define \"stuck\""
         (lambda () (interactive) (org-agenda nil "!")))
        ("C" "Configure custom agenda views"
         (lambda () (interactive) (org-agenda nil "C")))]
       ["Custom agendas"
        ("A" "Daily and overview"
         (lambda () (interactive) (org-agenda nil "A")))
        ("H" "Habits tracker"
         (lambda () (interactive) (org-agenda nil "H")))]])

    (transient-define-prefix org-capture-transient ()
      "Org Capture Templates"
      ["Capture"
       ("l" "Logged completed task" (lambda () (interactive) (org-capture nil "l")))
       ("n" "Note" (lambda () (interactive) (org-capture nil "n")))
       ("t" "Todo" (lambda () (interactive) (org-capture nil "t")))]))
  (add-hook 'emacs-startup-hook #'org-setup-transient-interface))
(add-hook 'org-agenda-mode-hook
          (lambda ()
            (add-hook 'auto-save-hook 'org-save-all-org-buffers nil t)
            (auto-save-mode)))

(use-package org-menu
  :ensure (:host github :repo "sheijk/org-menu")
  :after org
  :bind (:map org-mode-map
              ("C-o" . 'org-menu)))

(use-package ox-md
  :ensure nil
  :after org)

(use-package gist)

(blink-cursor-mode -1)
(global-auto-revert-mode)
(recentf-mode)
(savehist-mode)

(defun bedrock--backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* ((backupRootDir (concat user-emacs-directory "emacs-backup/"))
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))
(setq make-backup-file-name-function 'bedrock--backup-file-name)


(use-package pandoc-transient
  :ensure (:host github :repo "lispandfound/pandoc-transient")
  :bind (("C-c P" . pandoc-convert-transient)))


(use-package apheleia
  :init
  (apheleia-global-mode +1)
  :config
  (setf (alist-get 'toml-ts-mode apheleia-mode-alist) 'taplo)
  (setf (alist-get 'taplo apheleia-formatters)
        '("taplo" "format" filepath))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
        '(ruff ruff-isort))
  (setf (alist-get 'fourmolu apheleia-formatters)
        '("fourmolu" filepath))
  (setf (alist-get 'haskell-mode apheleia-mode-alist)
        'fourmolu)
  )

(defun clone-buffer-other-window ()
  (interactive)
  (switch-to-buffer-other-window (current-buffer)))

(global-set-key (kbd "C-c b") #'clone-buffer-other-window)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package transpose-frame
  :bind (("C-x 4 x" . transpose-frame)
         ("C-x 4 t" . rotate-frame)))

(use-package embrace
  :bind ("C-," . #'embrace-commander))

(use-package expreg
  :init
  (defvar-keymap expreg-expand-repeat-map
    :doc "Repeatedly expand selection up tree sitter nodes."
    :repeat t
    "<return>" #'expreg-expand
    "-" #'expreg-contract)
  (bind-key "<C-return>" #'expreg-expand))


(use-package ligature
  :demand t
  :config
  (ligature-set-ligatures 'prog-mode '("--" "---" "==" "===" "!=" "!==" "=!="
                                       "=:=" "=/=" "<=" ">=" "&&" "&&&" "&=" "++" "+++" "***" ";;" "!!"
                                       "??" "???" "?:" "?." "?=" "<:" ":<" ":>" ">:" "<:<" "<>" "<<<" ">>>"
                                       "<<" ">>" "||" "-|" "_|_" "|-" "||-" "|=" "||=" "##" "###" "####"
                                       "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#=" "^=" "<$>" "<$"
                                       "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</" "</>" "/>" "<!--" "<#--"
                                       "-->" "->" "->>" "<<-" "<-" "<=<" "=<<" "<<=" "<==" "<=>" "<==>"
                                       "==>" "=>" "=>>" ">=>" ">>=" ">>-" ">-" "-<" "-<<" ">->" "<-<" "<-|"
                                       "<=|" "|=>" "|->" "<->" "<~~" "<~" "<~>" "~~" "~~>" "~>" "~-" "-~"
                                       "~@" "[||]" "|]" "[|" "|}" "{|" "[<" ">]" "|>" "<|" "||>" "<||"
                                       "|||>" "<|||" "<|>" "..." ".." ".=" "..<" ".?" "::" ":::" ":=" "::="
                                       ":?" ":?>" "//" "///" "/*" "*/" "/=" "//=" "/==" "@_" "__" "???"
                                       "<:<" ";;;"))
  (global-ligature-mode t))

(use-package just-mode)

(use-package devdocs
  :bind ("C-h D" . devdocs-lookup))




(use-package ascii-art-to-unicode)

(repeat-mode)

(defun hl-todo-and-notes ()
  (font-lock-add-keywords nil'(("\\<\\(TODO\\|NOTE\\):" 1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook #'hl-todo-and-notes)

(use-package edit-indirect)

(use-package mermaid-mode)

(use-package markdown-mode
  :bind (:map markdown-mode-map
              ("<C-return>" . markdown-insert-header-like-org))
  :mode ("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . gfm-mode)
  :init
  (defun markdown-insert-header-like-org ()
    (interactive)
    (let ((outline-regexp "[#]+"))
      (outline-insert-heading))))

(use-package visual-fill-column
  :hook (gfm-mode))

(use-package cylc-mode
  :ensure (cylc-mode
           :host github
           :repo "cylc/cylc-flow"
           :files ("cylc/flow/etc/syntax/cylc-mode.el"))
  :mode ("suite.*\\.rc\\'" "\\.cylc\\'"))


(use-package grip-mode
  :bind (:map markdown-mode-command-map
              ("g" . grip-mode)))

;; The default bind for query-replace-regexp is stupid... C-M-%... who thought that was less useful than `move-to-window-line'!
(bind-key "M-r" #'query-replace-regexp)

(use-package ox-reveal
  :after org
  :init (require 'ox-reveal))


(setq sentence-end-double-space nil)

(defun to-snake-case (start end)
  "Change selected text to snake case format"
  (interactive "r")
  (if (use-region-p)
      (let ((camel-case-str (buffer-substring start end)))
        (delete-region start end)
        (insert (s-snake-case camel-case-str)))
    (message "No region selected")))


(use-package ibuffer
  :ensure nil
  :hook (ibuffer-mode . ibuffer-auto-mode))

(use-package all-the-icons-ibuffer
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

(use-package casual-ibuffer
  :ensure nil
  :bind (:map
         ibuffer-mode-map
         ("C-o" . casual-ibuffer-tmenu)
         ("F" . casual-ibuffer-filter-tmenu)
         ("s" . casual-ibuffer-sortby-tmenu)
         ("<double-mouse-1>" . ibuffer-visit-buffer) ; optional
         ("M-<double-mouse-1>" . ibuffer-visit-buffer-other-window) ; optional
         ("{" . ibuffer-backwards-next-marked) ; optional
         ("}" . ibuffer-forward-next-marked)   ; optional
         ("[" . ibuffer-backward-filter-group) ; optional
         ("]" . ibuffer-forward-filter-group)  ; optional
         ("$" . ibuffer-toggle-filter-group))  ; optional
  :after (ibuffer))

(use-package re-builder
  :ensure nil
  :defer t)

(use-package casual-re-builder
  :ensure nil
  :bind (:map
         reb-mode-map ("C-o" . casual-re-builder-tmenu)
         :map
         reb-lisp-mode-map ("C-o" . casual-re-builder-tmenu))
  :after (re-builder))

(use-package casual-bookmarks
  :ensure nil
  :bind (:map bookmark-bmenu-mode-map
              ("C-o" . casual-bookmarks-tmenu)
              ("S" . casual-bookmarks-sortby-tmenu)
              ("J" . bookmark-jump))
  :after (bookmark))

(use-package casual-agenda
  :ensure nil
  :after org-agenda  ;; Ensure org-agenda is loaded first
  :bind (:map
         org-agenda-mode-map
         ("C-o" . casual-agenda-tmenu)
         ("M-j" . org-agenda-clock-goto) ; optional
         ("J" . bookmark-jump))) ; optional

(use-package casual-editkit
  :ensure nil
  :bind (("C-c C-h" . casual-editkit-main-tmenu))
  :hook (rectangle-mark-mode . (lambda ()
                                 (define-key rectangle-mark-mode-map (kbd "C-o") #'casual-editkit-rectangle-tmenu))))


(use-package popper
  :bind (("C-=" . popper-toggle)
         (:repeat-map popper-toggle-repeat-map
                      ("=" . popper-cycle)
                      ("-" . popper-toggle)
                      ("t" . popper-toggle-type)))
  :demand t
  :config
  (popper-mode))



(use-package winner
  :ensure nil
  :bind (("s-/" . winner-undo)
         ("s-?" . winner-redo))
  :demand t
  :config (winner-mode))


(use-package apptainer-mode
  :ensure (:host github :repo "jrgant/apptainer-mode")
  :mode ("\\.def\\'" . apptainer-mode))


(use-package tldr
  :bind ("C-h t" . tldr)
  :init (add-to-list 'display-buffer-alist '("\\*tldr\\*" (display-buffer-in-side-window (side . bottom)))))


(use-package detached
  :init
  (detached-init)
  (add-to-list 'display-buffer-alist '("\\*Detached Shell Command\\*" (display-buffer-in-side-window (side . bottom))))
  :bind (;; Replace `async-shell-command' with `detached-shell-command'
         ([remap async-shell-command] . detached-shell-command)
         ;; Replace `compile' with `detached-compile'
         ([remap compile] . detached-compile)
         ([remap recompile] . detached-compile-recompile)
         ;; Replace built in completion of sessions with `consult'
         ([remap detached-open-session] . detached-consult-session))

  :custom ((detached-show-output-on-attach t)
           (detached-terminal-data-command system-type)))



(use-package dwim-shell-command
  :bind (([remap shell-command] . dwim-shell-command)
         :map dired-mode-map
         ([remap dired-do-async-shell-command] . dwim-shell-command)
         ([remap dired-do-shell-command] . dwim-shell-command)
         ([remap dired-smart-shell-command] . dwim-shell-command)))


(use-package dwim-shell-commands
  :ensure nil
  :after dwim-shell-command)

(add-to-list 'display-buffer-alist
             '("^\\*vc-git" display-buffer-no-window (allow-no-window . t)))

(bind-key "M-/" #'hippie-expand)
(global-prettify-symbols-mode)


(use-package uniline
  :ensure t)


(use-package envrc
  :demand t
  :init  (envrc-global-mode))

(use-package jenkinsfile-mode)


(use-package jinx
  :custom (jinx-languages "en_AU")
  :hook (emacs-startup . global-jinx-mode)
  :bind (([remap ispell-word] . jinx-correct)
         ("C-M-$" . jinx-languages))
  :config
  (with-eval-after-load 'embark
    (dolist (map '(embark-symbol-map embark-identifier-map))
      (bind-key "$" #'jinx-correct-word map))))

(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode))

(setq initial-major-mode 'text-mode)

(use-package gitignore-templates)
(use-package nix-ts-mode
  :mode "\\.nix\\'"

  )
(use-package org-download
  :after org
  :bind
  (:map org-mode-map
        (("s-Y" . org-download-screenshot)
         ("s-y" . org-download-yank))))


(use-package lookup
  :ensure nil
  :bind (("C-c l" . lookup/query))
  :load-path "lisp/"
  :init
  ;; Embark integration
  (with-eval-after-load 'embark
    (define-key embark-symbol-map (kbd "l") #'lookup/query))
  :config
  (add-hook 'kill-emacs-hook #'lookup/save-query-history))


;;; casual/config.el -*- lexical-binding: t; -*-

(use-package casual)


(use-package casual-calc
  :ensure nil
  :bind (:map calc-mode-map ("C-o" . casual-calc-tmenu))
  :after (calc))

(use-package casual-info
  :ensure nil
  :bind (:map Info-mode-map ("C-o" . casual-info-tmenu))
  :after (info))

(use-package casual-dired
  :ensure nil
  :bind (:map dired-mode-map ("C-o" . casual-dired-tmenu))
  :after (dired transient))

(use-package avy
  :bind (
         ;; From "Goto Thing" section
         ("M-g c" . avy-goto-char-timer) ; Character
         ;; Note: casual-avy-avy-goto-char-2, casual-avy-avy-goto-word-1,
         ;; casual-avy-avy-goto-symbol-1, casual-avy-avy-goto-whitespace-end,
         ;; and casual-avy-avy-goto-line are wrappers for avy functions
         ;; that handle --above/--below. To bind directly, use the base avy func.
         ("M-g 2" . avy-goto-char-2)      ; 2 Characters
         ("M-g w" . avy-goto-word-1)      ; Word
         ("M-g s" . avy-goto-symbol-1)    ; Symbol
         ("M-g W" . avy-goto-whitespace-end) ; Whitespace end
         ("M-g p" . avy-pop-mark)         ; Pop mark

         ;; From "Goto Line" section
         ("M-g l" . avy-goto-line)        ; Line
         ("M-g e" . avy-goto-end-of-line) ; End of line
         ("M-g o" . avy-org-goto-heading-timer) ; Org heading (requires org-mode)

         ;; From "Edit Other Line" section
         ("M-g C" . avy-kill-ring-save-whole-line) ; Copy whole line
         ("M-g k" . avy-kill-whole-line)  ; Kill whole line
         ("M-g m" . avy-move-line)        ; Move line
         ("M-g d" . avy-copy-line)        ; Duplicate line

         ;; From "Edit Other Region" section
         ("M-g r" . avy-kill-ring-save-region) ; Copy region
         ("M-g K" . avy-kill-region)      ; Kill region
         ("M-g M" . avy-move-region)      ; Move region
         ("M-g D" . avy-copy-region)      ; Duplicate region
         ("M-g t" . avy-transpose-lines-in-region) ; Transpose lines in region
         ("C-'" . avy-goto-symbol-1)
         ("C-c C-j" . avy-resume)
         )
  :custom (avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)))


(use-package casual-make
  :ensure nil
  :bind (:map makefile-mode-map ("C-o" . casual-make-tmenu))
  :after (make-mode))

(use-package casual-isearch
  :ensure nil
  :bind (:map isearch-mode-map ("C-o" . casual-isearch-tmenu)))

(use-package casual-ibuffer
  :ensure nil
  :bind (:map
         ibuffer-mode-map
         ("C-o" . casual-ibuffer-tmenu)
         ("F" . casual-ibuffer-filter-tmenu)
         ("s" . casual-ibuffer-sortby-tmenu)
         ("<double-mouse-1>" . ibuffer-visit-buffer) ; optional
         ("M-<double-mouse-1>" . ibuffer-visit-buffer-other-window) ; optional
         ("{" . ibuffer-backwards-next-marked) ; optional
         ("}" . ibuffer-forward-next-marked)   ; optional
         ("[" . ibuffer-backward-filter-group) ; optional
         ("]" . ibuffer-forward-filter-group)  ; optional
         ("$" . ibuffer-toggle-filter-group))  ; optional
  :after (ibuffer))

(use-package casual-image
  :ensure nil
  :bind (:map image-mode-map ("C-o" . casual-image-tmenu)))

(use-package casual-bookmarks
  :ensure nil
  :bind (:map bookmark-bmenu-mode-map
              ("C-o" . casual-bookmarks-tmenu)
              ("S" . casual-bookmarks-sortby-tmenu)
              ("J" . bookmark-jump))
  :after (bookmark))

(use-package casual-agenda
  :ensure nil
  :after org-agenda  ;; Ensure org-agenda is loaded first
  :bind (:map
         org-agenda-mode-map
         ("C-o" . casual-agenda-tmenu)
         ("M-j" . org-agenda-clock-goto) ; optional
         ("J" . bookmark-jump))) ; optional

(use-package casual-editkit
  :ensure nil
  :bind (("C-c C-h" . casual-editkit-main-tmenu))
  :hook (rectangle-mark-mode . (lambda ()
                                 (define-key rectangle-mark-mode-map (kbd "C-o") #'casual-editkit-rectangle-tmenu))))
(use-package magit-lfs
  :after (magit))
(use-package cmake-mode)

(use-package pet
  :init
  (defun pet/initialise-environment ()
    (interactive)
    (setq-local python-shell-interpreter (or (pet-executable-find "ipython") (pet-executable-find "python"))
                python-shell-virtualenv-root (pet-virtualenv-root)
                eglot-server-programs `(((python-base-mode python-mode python-ts-mode) ,(pet-executable-find "basedpyright-langserver") "--stdio")))
    (setq-local apheleia-formatters `((ruff ,(pet-executable-find "ruff") "format" "--silent"
                                            (apheleia-formatters-fill-column "--line-length")
                                            "--stdin-filename" filepath "-")
                                      (ruff-isort ,(pet-executable-find "ruff") "check" "-n" "--select" "I" "--fix" "--fix-only"
                                                  "--stdin-filename" filepath "-")))
  (add-hook 'python-base-mode-hook 'pet-mode -10)
  (add-hook 'python-base-mode-hook #'pet/initialise-environment))

(use-package eglot-booster
  :after eglot
  :ensure (:host github :repo "jdtsmith/eglot-booster")
  :config	(eglot-booster-mode))

(use-package deadgrep
  :bind (("<f5>" . #'deadgrep)
         ("C-x p g" . #'deadgrep)))

(setq remote-file-name-inhibit-locks t
      tramp-use-scp-direct-remote-copying t
      remote-file-name-inhibit-auto-save-visited t
      tramp-copy-size-limit (* 1024 1024)
      tramp-verbose 2)

(connection-local-set-profile-variables
 'remote-direct-async-process
 '((tramp-direct-async-process . t)))


(connection-local-set-profile-variables
 '(:application tramp :protocol "scp")
 'remote-direct-async-process)

(setq magit-tramp-pipe-stty-settings 'pty)


(with-eval-after-load 'tramp
  (with-eval-after-load 'compile
    (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options)))

(use-package rust-mode)


(use-package flymake-collection)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(setq bookmark-save-flag 1)


(use-package edit-server
  :commands edit-server-start
  :init (if after-init-time
            (edit-server-start)
          (add-hook 'after-init-hook
                    #'(lambda() (edit-server-start))))
  :config (setq edit-server-new-frame-alist
                '((name . "Edit with Emacs FRAME")
                  (top . 200)
                  (left . 200)
                  (width . 80)
                  (height . 25)
                  (minibuffer . t)
                  (menu-bar-lines . t))))
