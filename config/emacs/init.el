;;; init.el --- Emacs Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Personal Emacs configuration with modern packages and workflow enhancements.

;;; Code:

;; ============================================================================
;; CUSTOM FILE
;; ============================================================================

(setopt custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; ============================================================================
;; PACKAGE MANAGER - Native use-package (Emacs 29+)
;; ============================================================================

(require 'use-package)
(require 'package)

;; Configure package archives
(setopt package-archives
        '(("melpa" . "https://melpa.org/packages/")
          ("gnu" . "https://elpa.gnu.org/packages/")
          ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)

;; Ensure use-package always installs missing packages
(setopt use-package-always-ensure t)

;; ============================================================================
;; UI & APPEARANCE
;; ============================================================================

;; Enable modern smooth scrolling (Emacs 29+)
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

(use-package transient
  :demand t)

(use-package modus-themes
  :demand t
  :config
  (defun jake/pad-mode-line (&rest _)
    ;; From the modus manual
    (modus-themes-with-colors
      (custom-set-faces
       `(mode-line-active ((,c :box (:line-width 5 :color ,bg-mode-line-active))))
       `(mode-line-inactive ((,c :box (:line-width 5 :color ,bg-mode-line-inactive)))))))
  (add-hook 'modus-themes-after-load-theme-hook #'jake/pad-mode-line)
  (modus-themes-load-theme 'modus-operandi))

(use-package mood-line
  :demand t
  :config
  (require 'mood-line-segment-vc)
  (mood-line-mode))

;; Display time in the mode line
(display-time-mode)

;; ============================================================================
;; KEYBINDINGS & INPUT
;; ============================================================================

;; Swap C-t and C-x for more ergonomic keybindings
(defun jake/setup-keyboard-swap ()
  "Setup C-t and C-x keyboard swap."
  (keyboard-translate ?\C-t ?\C-x)
  (keyboard-translate ?\C-x ?\C-t))

(jake/setup-keyboard-swap)
(add-hook 'server-after-make-frame-hook #'jake/setup-keyboard-swap)
(global-unset-key (kbd "C-t"))          ; unbind the transpose-char key because it annoys me

;; ============================================================================
;; COMPLETION FRAMEWORK
;; ============================================================================

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
  :custom
  (consult-imenu-config
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
  (register-preview-function #'consult-register-format)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)



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

;; ----------------------------------------------------------------------------
;; Better Defaults & Basic Editor Behavior
;; ----------------------------------------------------------------------------

(use-package emacs
  :custom
  (uniquify-buffer-name-style 'forward) ; Distinguish buffers with same name

  ;; --- Editor Behavior ---
  (save-place-mode 1)               ; Remember cursor position in files
  (indent-tabs-mode nil)            ; Use spaces, not tabs
  (require-final-newline t)         ; Always end files with a newline
  (load-prefer-newer t)             ; Don't load stale .elc files
  (visible-bell t)                  ; Flash instead of beep

  ;; --- Smooth Workflows ---
  (save-interprogram-paste-before-kill t) ; Save clipboard to kill-ring before replacing
  (mouse-yank-at-point t)           ; Paste where cursor is, not where mouse is
  (ediff-window-setup-function 'ediff-setup-windows-plain) ; No extra Ediff frames

  :bind
  ;; 'zap-to-char' kills the char; 'zap-up-to-char' kills everything BEFORE it.
  ("M-z" . zap-up-to-char))

(add-hook 'after-init-hook #'electric-pair-mode)
(add-hook 'after-init-hook #'electric-indent-mode)

(defun jake/rebind-comment-new-line ()
  "Rebind RET to comment-indent-new-line in prog-mode."
  (local-set-key (kbd "RET") #'comment-indent-new-line)
  (local-set-key (kbd "<S-return>") #'newline-and-indent))

(add-hook 'prog-mode-hook #'jake/rebind-comment-new-line)

(use-package which-key
  :demand t
  :config
  (which-key-mode))

(repeat-mode)

(defun jake/hl-todo-and-notes ()
  "Highlight TODO and NOTE keywords in prog-mode."
  (font-lock-add-keywords nil'(("\\<\\(TODO\\|NOTE\\):" 1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook #'jake/hl-todo-and-notes)

(use-package edit-indirect
  :defer t)

(defun jake/clone-buffer-other-window ()
  "Clone current buffer in another window."
  (interactive)
  (switch-to-buffer-other-window (current-buffer)))

(global-set-key (kbd "C-c b") #'jake/clone-buffer-other-window)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package transpose-frame
  :bind (("C-x 4 x" . transpose-frame)
         ("C-x 4 t" . rotate-frame)))

(use-package embrace
  :bind ("C-," . embrace-commander))

(use-package expreg
  :bind
  (("<C-return>" . expreg-expand)
   (:repeat-map expreg-expand-repeat-map
                ("<return>" . expreg-expand)
                ("-"  . expreg-contract))))


(use-package ligature
  :hook (prog-mode)
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
                                       "<:<" ";;;")))

;; ----------------------------------------------------------------------------
;; Corfu - In-buffer completion popup
;; ----------------------------------------------------------------------------

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
  (global-corfu-mode)
  (with-eval-after-load 'python
    (add-hook 'inferior-python-mode-hook (lambda () (setq-local corfu-auto nil)))))


;; ----------------------------------------------------------------------------
;; Orderless - Completion matching style
;; ----------------------------------------------------------------------------

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))


;; ----------------------------------------------------------------------------
;; Marginalia - Rich annotations in minibuffer
;; ----------------------------------------------------------------------------

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

;; ----------------------------------------------------------------------------
;; Vertico - Vertical completion UI
;; ----------------------------------------------------------------------------

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
  :bind (:map vertico-map
              ("RET"  . vertico-directory-enter)
              ("DEL"  . vertico-directory-delete-char)
              ("M-DEL"  . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay-hook . vertico-directory-tidy))


;; ----------------------------------------------------------------------------
;; Emacs built-in enhancements for completion
;; ----------------------------------------------------------------------------

;; A few more useful configurations...
(use-package emacs
  :ensure nil
  :custom
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun jake/crm-indicator (args)
    "Add CRM indicator to completing-read-multiple."
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'jake/crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

;; TAB completion and indentation settings
(use-package emacs
  :ensure nil
  :custom
  (tab-always-indent 'complete)
  (tab-first-completion nil)
  (text-mode-ispell-word-completion nil)
  (read-extended-command-predicate #'command-completion-default-include-p))


;; ----------------------------------------------------------------------------
;; Cape - Completion At Point Extensions
;; ----------------------------------------------------------------------------

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
  ;;(add-hook 'completion-at-point-functions #'cape-sgml)
  ;;(add-hook 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-hook 'completion-at-point-functions #'cape-abbrev)
  ;;(add-hook 'completion-at-point-functions #'cape-dict)
  ;;(add-hook 'completion-at-point-functions #'cape-elisp-symbol)
  )

;; ----------------------------------------------------------------------------
;; Embark - Act on targets at point
;; ----------------------------------------------------------------------------

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

;; ============================================================================
;; VERSION CONTROL
;; ============================================================================

(use-package magit
  :bind (("C-x g" . magit-status)
         :map git-commit-mode-map
         ("C-c C-g" . copilot-chat-insert-commit-message))
  :init (with-eval-after-load 'project
          (add-to-list 'project-switch-commands '(magit-project-status "Magit" "m"))))

(use-package forge
  :bind ("C-c '" . forge-dispatch))

(use-package git-link
  :bind (("C-c g l" . git-link)))

(use-package pr-review
  :after magit
  :init
  (defun jake/pr-review-via-forge ()
    (interactive)
    (if-let* ((target (forge--browse-target))
              (url (if (stringp target) target (forge-get-url target)))
              (rev-url (pr-review-url-parse url)))
        (pr-review url)
      (user-error "No PR to review at point"))))

(use-package gitignore-templates)

(use-package magit-lfs
  :after (magit))

(use-package gist)

;; ============================================================================
;; PROGRAMMING MODES & LANGUAGE SUPPORT
;; ============================================================================

;; ----------------------------------------------------------------------------
;; Python Environment Management
;; ----------------------------------------------------------------------------

(use-package pyvenv)

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :demand t
  :config
  (global-treesit-auto-mode)
  (treesit-auto-add-to-auto-mode-alist 'all))

;; ----------------------------------------------------------------------------
;; Python Configuration
;; ----------------------------------------------------------------------------

(use-package python
  :ensure nil
  :bind (:map python-ts-mode-map
              (:repeat-map python-indent-shift-right-repeat-map
                           (">" . python-indent-shift-right)
                           ("<" . python-indent-shift-left)))

  :custom
  (major-mode-remap-alist '((python-mode . python-ts-mode)))
  (python-shell-completion-native-enable nil)

  :config
  (defun jake/python-prompt-with (packages)
    "Spawn an IPython REPL with extra packages injected via `uv --with`.

Interactively, PACKAGES comes from `completing-read-multiple`.
If invoked with `C-u`, also prompt for a Python version to pin."
    (interactive (list (completing-read-multiple "Packages: " nil)))
    (let* ((uv-exec (or (executable-find "uv")
                        (user-error "uv not found in PATH")))
           ;; optional version, only when user gave C-u
           (python-version (when current-prefix-arg
                             (read-string "Python version (optional): ")))
           ;; Filter out empty entries
           (pkgs (seq-filter (lambda (s) (not (string-empty-p s))) packages))
           ;; Build the --with arguments, always include ipython
           (with-args
            (mapcan (lambda (p) (list "--with" p))
                    (cons "ipython" pkgs)))
           ;; Python version flag
           (version-args
            (if (and python-version (not (string-empty-p python-version)))
                (list "--python" python-version)
              '()))
           ;; Construct full shell command
           (cmd (string-join
                 (append (list uv-exec "run")
                         version-args
                         with-args
                         (list "ipython" "--simple-prompt"))
                 " "))
           (buf-name "UV Python"))
      (python-shell-make-comint cmd buf-name t)))
  (add-hook 'python-base-mode-hook
            (lambda ()
              (setq-local completion-at-point-functions
                          (remove #'python-completion-at-point
                                  completion-at-point-functions))))

  ;; --- Extra flymake checkers ---
  (defun jake/python-extra-flymake-backends ()
    "Enable additional Flymake checkers alongside Eglot’s."
    ;; Buffer-local so they affect only this buffer
    (add-hook 'flymake-diagnostic-functions
              'flymake-collection-uv-ruff nil t)
    (add-hook 'flymake-diagnostic-functions
              'flymake-collection-numpydoc nil t))

  (defun jake/uv-which (uv-exec exec)
    (when uv-exec
      (let ((path (car (process-lines uv-exec "run" "which" exec))))
        (when path
          (string-trim path)))))

  (defun jake/setup-python-environment ()
    "Configure python-shell to use `uv run which python/ipython`."
    (interactive)
    (let* ((uv-exec (executable-find "uv"))
           (ipython (jake/uv-which uv-exec "ipython"))
           (python (jake/uv-which uv-exec "python")))
      (cond
       ;; Prefer IPython if found
       ((and ipython (not (string-empty-p ipython)))
        (setq-local python-shell-interpreter ipython
                    python-shell-interpreter-args "--simple-prompt")
        (message "Using IPython: %s" ipython))
       ;; Otherwise fall back to Python
       ((and python (not (string-empty-p python)))
        (setq-local python-shell-interpreter python
                    python-shell-interpreter-args "")
        (message "Using Python: %s" python))
       ;; If uv or both executables missing
       (t
        (message "Could not determine python via uv.")))))



  (add-hook 'python-ts-mode-hook #'jake/setup-python-environment)
  ;; --- Formatter setup ---
  (add-hook 'python-ts-mode-hook
            (lambda ()
              (setq-local apheleia-formatters
                          `((ruff "uvx" "ruff" "format" "--silent"
                                  (apheleia-formatters-fill-column "--line-length")
                                  "--stdin-filename" filepath "-")
                            (ruff-isort "uvx" "ruff" "check" "-n" "--select" "I"
                                        "--fix" "--fix-only"
                                        "--stdin-filename" filepath "-")))))

  ;; --- Eglot server setup ---
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(python-ts-mode "uv" "run" "--extra" "types" "--extra" "dev" "ty" "server")))


  ;; ------------------------------------------------------------------
  ;; 🔥 The important part: stay out of flymake, then re-add eglot’s backend
  ;; ------------------------------------------------------------------

  ;; Step 1: tell Eglot NOT to install its flymake backend initially
  ;; Step 2: start eglot
  (add-hook 'python-ts-mode-hook
            (lambda ()
              (setq-local eglot-stay-out-of '(flymake))
              (eglot-ensure)))

  ;; Step 3: AFTER eglot starts, manually re-add eglot's flymake backend
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (when (derived-mode-p 'python-ts-mode)
                ;; eglot uses eglot-flymake-backend for diagnostics
                (setq-local flymake-diagnostic-functions nil)
                (add-hook 'flymake-diagnostic-functions
                          #'eglot-flymake-backend
                          nil t)
                ;; and then add your extra checkers
                (jake/python-extra-flymake-backends))))

  ;; --- misc python-mode things ---
  (add-hook 'python-ts-mode-hook
            (lambda ()
              (setq-local transpose-sexps-function #'treesit-transpose-sexps
                          python-shell-interpreter-args "--simple-prompt --classic"
                          devdocs-current-docs '("pandas~2"
                                                 "numpy~2.0"
                                                 "python~3.13"
                                                 "matplotlib")))))

(use-package uv
  :ensure nil
  :load-path "lisp/"
  :after (python)
  :bind (:map python-ts-mode-map
              ("C-c u" . uv-menu)))

(use-package cython-mode)

;; ----------------------------------------------------------------------------
;; Python documentation helper
;; ----------------------------------------------------------------------------

(use-package python-numpydoc
  :ensure nil
  :after (python)
  :load-path "lisp/"
  :bind (:map python-ts-mode-map
              ("M-n" . python-numpydoc-insert)))

(use-package comint-mime
  :hook ((shell-mode . comint-mime-setup)
         (inferior-python-mode . comint-mime-setup)))

;; ----------------------------------------------------------------------------
;; Additional Language Modes
;; ----------------------------------------------------------------------------

(use-package yaml-mode
  :mode ("\\.cff\\'" . yaml-ts-mode)
  :init
  (defun jake/yaml-mode-setup-flymake ()
    (add-hook 'flymake-diagnostic-functions 'flymake-collection-yamllint nil t)
    (flymake-mode +1))

  (add-hook 'yaml-mode-hook #'jake/yaml-mode-setup-flymake)
  (add-hook 'yaml-ts-mode-hook #'jake/yaml-mode-setup-flymake))

(use-package csv-mode
  :hook (csv-mode . csv-align-mode))

(use-package haskell-mode
  :bind (:map haskell-mode-map
              (("C-c C-c" . haskell-compile))))

(use-package rust-ts-mode
  :ensure nil
  :hook (rust-ts-mode . eglot-ensure))

(use-package cylc-mode
  :vc (:url "https://github.com/cylc/cylc-flow"
            :rev :last-vc
            :lisp-dir "cylc/flow/etc/syntax/")
  :mode ("suite.*\\.rc\\'" "\\.cylc\\'"))

(use-package apptainer-mode
  :vc (:url "https://github.com/jrgant/apptainer-mode")
  :mode ("\\.def\\'" . apptainer-mode))

(use-package envrc
  :demand t
  :init  (envrc-global-mode))

(use-package elm-mode)

(use-package just-mode)

(use-package nix-ts-mode
  :mode "\\.nix\\'")

(use-package jenkinsfile-mode)
(use-package mermaid-mode)
(use-package cmake-mode)

;; ============================================================================
;; TEXT EDITING & TEMPLATES
;; ============================================================================

(global-set-key [remap newline] #'newline-and-indent)

(defun jake/local-disable-line-numbers ()
  (setq-local display-line-numbers nil))

;; ----------------------------------------------------------------------------
;; Tempel - Simple template system
;; ----------------------------------------------------------------------------

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

(use-package project
  :ensure nil
  :bind (("C-x p t" . jake/project-test) ("C-x p i" . consult-imenu-multi))
  :custom
  (project-test-command "just test")
  :init
  (defun jake/project-test ()
    "Run project test command."
    (interactive)
    (let ((compile-command project-test-command))
      (call-interactively #'project-compile))))




(defun jake/smarter-move-beginning-of-line (arg)
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

;; remap C-a to `jake/smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'jake/smarter-move-beginning-of-line)

(setopt dired-dwim-target t
        dired-auto-revert-buffer t
        dired-listing-switches "-alFh"
        isearch-lazy-count t
        tramp-use-ssh-controlmaster-options nil
        vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)" vc-ignore-dir-regexp tramp-file-name-regexp)
        use-short-answers t)

;; ----------------------------------------------------------------------------
;; Display Buffer Configuration
;; ----------------------------------------------------------------------------
(use-package popper
  :bind (("C-=" . popper-toggle)
         (:repeat-map popper-toggle-repeat-map
                      ("=" . popper-cycle)
                      ("-" . popper-toggle)
                      ("t" . popper-toggle-type)))
  :demand t
  :config
  (popper-mode))

(add-to-list 'display-buffer-alist
             '("^\\*vc-git" display-buffer-no-window (allow-no-window . t)))

;; ----------------------------------------------------------------------------
;; Environment & Path Management
;; ----------------------------------------------------------------------------

(use-package exec-path-from-shell
  :demand t
  :config
  (exec-path-from-shell-initialize))

(use-package dumb-jump
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(delete-selection-mode)

(use-package comment-dwim-2
  :bind ("M-;" . comment-dwim-2))

(use-package crux
  :bind (("C-k" . crux-smart-kill-line)
         ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
         ("C-c S" . crux-find-user-init-file)
         ("C-c o" . crux-open-with)
         ("M-o" . crux-other-window-or-switch-buffer)
         ("C-c n" . crux-cleanup-buffer-or-region)
         ("C-c D" . crux-find-directory-dir-locals-file)
         ("C-g" . crux-keyboard-quit-dwim)
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
  (defun jake/eshell-previous-directories ()
    "Return list of previous eshell directories."
    (delete-dups (mapcar 'abbreviate-file-name
                         (ring-elements eshell-last-dir-ring))))

  (defun eshell/z (&optional regexp)
    "Navigate to a previously visited directory in eshell."
    (cond
     ((and (not regexp) (featurep 'consult-dir))
      (eshell/cd (substring-no-properties (consult-dir--pick "Switch directory: "))))
     (t (eshell/cd (if regexp (eshell-find-previous-directory regexp)
                     (completing-read "cd: " (jake/eshell-previous-directories)))))))

  (defvar consult-dir--source-eshell `(:name "Eshell"
                                             :narrow ?e
                                             :category file
                                             :face consult-file
                                             :enabled ,(lambda () (and (boundp 'eshell-last-dir-ring) eshell-last-dir-ring))
                                             :items ,#'jake/eshell-previous-directories))

  (defun jake/consult-dir-zoxide-dirs ()
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
                :items    ,#'jake/consult-dir-zoxide-dirs)
    "Zoxide directory source for `consult-dir'.")

  :config
  (add-to-list 'consult-dir-sources 'consult-dir--source-eshell t)
  (add-to-list 'consult-dir-sources 'consult-dir--source-zoxide t))

(use-package wgrep)

;; ============================================================================
;; ORG MODE & PRODUCTIVITY
;; ============================================================================

(use-package org
  :bind (:map org-mode-map
              ("C-c M-o" . jake/open-note-in-browser)
              ("M-<return>" . org-meta-return)) ;; required because of crux override.
  :hook
  (org-mode . visual-wrap-prefix-mode)
  :custom
  (org-capture-templates
   '(("l" "Logged completed task" entry
      (file+headline org-agenda-capture-file "Tasks")
      "* DONE %?\12 %U\12 %a\12 %i")
     ("n" "Note" entry (file+headline org-default-notes-file "Notes")
      "* %?\12 %U\12 %a\12 %i")
     ("t" "Todo" entry (file+headline org-agenda-capture-file "Tasks")
      "* TODO %?\12 %U\12 %a\12 %i")))
  (org-directory "~/Sync")
  (org-todo-keywords '((sequence "TODO" "WAIT(w@/!)" "|" "DONE" "KILL")))
  :init

  (setq org-file-apps-gnu '((remote . emacs) (system . "xdg-open \"%s\"") (t  . "xdg-open \"%s\"")))
  ;; (add-hook 'org-agenda-mode-hook
  ;;           (lambda ()
  ;;             (add-hook 'auto-save-hook 'org-save-all-org-buffers nil t)
  ;;             (auto-save-mode)))
  ;;
  :config
  (add-to-list 'org-preview-latex-process-alist
               '(tectonic :programs ("tectonic" "convert")
                          :description "pdf > png"
                          :message "you need install the programs: tectonic and imagemagick."
                          :image-input-type "pdf"
                          :image-output-type "png"
                          :image-size-adjust (1.0 . 1.0)
                          :latex-compiler
                          ("tectonic -Z shell-escape-cwd=%o --outfmt pdf --outdir %o %f")
                          :image-converter
                          ("convert -density %D -trim -antialias %f -quality 300 %O")))
  (setq org-preview-latex-default-process 'tectonic)
  (require 'ox-publish)

  (require 'ox-publish)

  (setq org-publish-project-alist
        `(
          ("lisp-notes-local"
           :base-directory "~/notes/"
           :base-extension "org"
           :publishing-directory "/tmp/notes/"
           :recursive t
           :publishing-function org-html-publish-to-html
           :auto-preamble t
           ;; Add this line to inject the CSS:
           :html-head "<link rel='stylesheet' type='text/css' href='/style.css'/>")

          ("lisp-static-local"
           :base-directory "~/notes/"
           :base-extension "png\\|jpg\\|gif\\|pdf\\|css"
           :publishing-directory "/tmp/notes/"
           :recursive t
           :publishing-function org-publish-attachment)
          ))

  ;; THE DEPLOY FUNCTION
  (defun jake/deploy-notes ()
    "Build everything locally, then use rsync filters to distribute to VPS."
    (interactive)
    (message "Building all notes locally...")
    (org-publish "lisp-notes-local" t)
    (org-publish "lisp-static-local" t)

    (message "Deploying to VPS...")

    ;; 1. Sync EVERYTHING to the PRIVATE folder (The 'Master' copy)
    (call-process-shell-command
     "rsync -avz --delete /tmp/notes/ jake@vps:/var/www/lispandfound.xyz/private/")

    ;; 2. Sync ONLY 'public' files to the PUBLIC folder
    ;; We include anything with '__' and 'public' in the name, and exclude everything else.
    (call-process-shell-command
     (concat "rsync -avz --delete "
             "--include='*__*public*.html' " ;; Keep public HTML
             "--include='*/' "               ;; Keep directories (to look inside them)
             "--exclude='*' "                ;; Exclude everything else
             "/tmp/notes/ jake@vps:/var/www/lispandfound.xyz/public/"))

    (message "Deployment successful!"))
  (defun jake/open-note-in-browser (&optional arg)
    "Open the private URL of the current note in a browser.
With a prefix ARG (C-u), copy the public URL to the kill ring instead."
    (interactive "P")
    (let* ((filename (file-name-nondirectory (buffer-file-name)))
           (basename (file-name-sans-extension filename))
           (url (concat "https://lispandfound.xyz/private/" basename ".html"))
           (public-url (concat "https://lispandfound.xyz/public/" basename ".html")))
      (if arg
          (progn
            (kill-new public-url)
            (message "Copied to clipboard: %s" public-url))
        (browse-url url))))

  )

(use-package org-transient
  :ensure nil
  :commands (org-agenda-transient org-capture-transient)
  :load-path "lisp/"
  :bind (("C-c a" . 'org-agenda-transient)
         ("C-c x" . 'org-capture-transient)))


(use-package org-present)


(use-package org-menu
  :vc (:url "https://github.com/sheijk/org-menu")
  :after org
  :bind (:map org-mode-map
              ("C-o" . 'org-menu)))

(use-package ox-md
  :ensure nil
  :after org)

(use-package ox-slack
  :after org)

(use-package org-download
  :after org
  :hook (dired-mode . org-download-enable)
  :bind
  (:map org-mode-map
        (("s-Y" . org-download-screenshot)
         ("s-y" . org-download-yank))))

(use-package ox-reveal
  :after org
  :init (require 'ox-reveal))

;; ============================================================================
;; NOTES & KNOWLEDGE MANAGEMENT
;; ============================================================================

;; ----------------------------------------------------------------------------
;; Literature Search
;; ----------------------------------------------------------------------------

;;;###autoload
(defun jake/consult-ripgrep-all (&optional dir initial)
  "Use ripgrep-all for searching with consult."
  (interactive "P")
  (let ((consult-ripgrep-args "rga --null --line-buffered --color=never --max-columns=1000  --smart-case --no-heading --with-filename --line-number"))
    (consult-ripgrep dir initial)))

(setopt literature-directory "~/Sync")

(defun jake/search-papers ()
  "Search papers using ripgrep-all in literature directory."
  (interactive)
  (jake/consult-ripgrep-all literature-directory))

(bind-key "s-p" #'jake/search-papers)

;; ----------------------------------------------------------------------------
;; Denote - Simple note-taking system
;; ----------------------------------------------------------------------------

(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode)
  :bind (("s-d" . jake/denote-transient))
  :custom
  (denote-directory (expand-file-name "~/notes/"))
  :init
  (transient-define-prefix jake/denote-transient ()
    "Denote dispatch"
    [["Note creation (d)"
      ("d" "new note" denote)
      ("j" "new or existing journal entry" denote-journal-new-or-existing-entry)
      ("n" "open or new" denote-open-or-create)
      ("t" "new specifying date and time" denote-date)
      ("+" "create in subdirectory " denote-subdirectory)]]
    [["Bookkeeping (b)"
      ("br" "prompt and rename" denote-rename-file)
      ("bf" "rename with frontmatter" denote-rename-file-using-front-matter)
      ("bk" "modify keywords" denote-rename-file-keywords)]
     ["Linking (l)"
      ("i" "insert link" denote-link)
      ("lh" "insert link to org heading" denote-org-link-to-heading)
      ("lb" "show backlinks" denote-backlinks)
      ("lg" "visit backlink" denote-find-backlink)
      ("lo" "org backlink block" denote-org-dblock-insert-backlinks)]]
    [["Searching (s)"
      ("sn" "consult-notes" consult-notes)
      ("ss" "consult-notes search" consult-notes-search-in-all-notes)]])
  :config
  ;; Automatically rename Denote buffers when opening them so that
  ;; instead of their long file name they have, for example, a literal
  ;; "[D]" followed by the file's title.  Read the doc string of
  ;; `denote-rename-buffer-format' for how to modify this.
  (denote-rename-buffer-mode 1))

(use-package org-fragtog
  :ensure t
  :hook (org-mode . org-fragtog-mode))

(use-package denote-journal
  :ensure t
  :after denote
  :custom
  (denote-journal-keyword "labnotes")
  (denote-journal-directory (expand-file-name "~/notes/labnotes"))
  (denote-journal-title-format 'day-date-month-year))
(use-package denote-org
  :after denote
  :custom
  ;; `denote-org-link-to-heading' controls the behavior of
  ;; `org-store-link': setting to id makes it insert an ID in the
  ;; PROPERTIES drawer
  ;;
  ;; (denote-org-store-link-to-heading 'context)
  (denote-org-store-link-to-heading 'id))


(use-package consult-notes
  :vc (:url "https://github.com/mclear-tools/consult-notes")
  :commands (consult-notes consult-notes-search-in-all-notes)
  :config
  ;; Set org-roam integration, denote integration, or org-heading integration e.g.:
  (consult-notes-org-headings-mode)
  (consult-notes-denote-mode)
  ;; search only for text files in denote dir
  (setopt consult-notes-denote-files-function (lambda () (denote-directory-files nil t t))))



;; ============================================================================
;; EDITOR BEHAVIOR & BUILT-IN SETTINGS
;; ============================================================================

(blink-cursor-mode -1)
(global-auto-revert-mode)
(recentf-mode)
(savehist-mode)

(setopt initial-major-mode 'text-mode)

(let ((backup-dir (concat user-emacs-directory "backups/"))
      (auto-save-dir (concat user-emacs-directory "auto-save/")))

  (unless (file-directory-p backup-dir)
    (make-directory backup-dir))

  (setq backup-directory-alist
        `((".*" . ,backup-dir)))

  (unless (file-directory-p auto-save-dir)
    (make-directory auto-save-dir))

  (setq auto-save-file-name-transforms
        `((".*" ,auto-save-dir t))))


;; ============================================================================
;; FILE CONVERSION & FORMATTING
;; ============================================================================

(use-package pandoc-transient
  :vc (:url "https://github.com/lispandfound/pandoc-transient")
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

;; ----------------------------------------------------------------------------
;; Markdown Support
;; ----------------------------------------------------------------------------

(use-package markdown-mode
  :bind (:map markdown-mode-map
              ("<C-return>" . markdown-insert-header-like-org))
  :mode ("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . gfm-mode)
  :hook (gfm-mode . visual-wrap-prefix-mode)
  :init
  (defun markdown-insert-header-like-org ()
    (interactive)
    (let ((outline-regexp "[#]+"))
      (outline-insert-heading))))


(use-package grip-mode
  :bind (:map markdown-mode-command-map
              ("g" . grip-mode)))

;; The default bind for query-replace-regexp is stupid... C-M-%... who thought that was less useful than `move-to-window-line'!
(bind-key "M-r" #'query-replace-regexp)

(setopt sentence-end-double-space nil)

(defun jake/to-snake-case (start end)
  "Change selected text to snake case format."
  (interactive "r")
  (if (use-region-p)
      (let ((camel-case-str (buffer-substring start end)))
        (delete-region start end)
        (insert (s-snake-case camel-case-str)))
    (message "No region selected")))

;; ============================================================================
;; BUFFER & WINDOW MANAGEMENT
;; ============================================================================

(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :hook (ibuffer-mode . ibuffer-auto-mode))

(use-package ibuffer-project
  :hook (ibuffer-mode . jake/ibuffer-project-group-order)
  :init
  (defun jake/ibuffer-project-group-order ()
    (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
    (unless (eq ibuffer-sorting-mode 'project-file-relative)
      (ibuffer-do-sort-by-project-file-relative))))

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






(use-package winner
  :ensure nil
  :bind (("s-/" . winner-undo)
         ("s-?" . winner-redo))
  :demand t
  :config (winner-mode))



(bind-key "M-/" #'hippie-expand)
(global-prettify-symbols-mode)


(use-package uniline
  :ensure t)

;; ============================================================================
;; SPELL CHECKING & LINTING
;; ============================================================================

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

(use-package flymake-collection
  :demand t
  :config
  (require 'flymake-collection-define)

  (flymake-collection-define-rx flymake-collection-numpydoc
    "Numpydoc documentation checker."
    :title "numpydoc"
    :pre-let ((numpydoc-exec (executable-find "numpydoc_wrapper")))
    :pre-check (unless numpydoc-exec
                 (error "Cannot find numpydoc wrapper"))
    :source-inplace t
    :write-type 'file
    :regexps ((error bol (file-name) ":" line ": " (id (* alnum)) " " (message) eol))
    :command `(,numpydoc-exec ,flymake-collection-temp-file))
  (flymake-collection-define-enumerate flymake-collection-uv-ruff
    "A Python syntax and style checker using Ruff.

See URL `https://github.com/charliermarsh/ruff'."
    :title "ruff"
    :pre-let ((uv-exec (executable-find "uvx")))
    :pre-check (unless uv-exec
                 (error "Cannot find uv executable"))
    :write-type 'pipe
    :command `(,uv-exec
               "ruff"
               "check"
               "--output-format" "json"
               ,@(when-let ((file (buffer-file-name flymake-collection-source)))
                   (list "--stdin-filename" file))
               "-")

    :generator
    (car (flymake-collection-parse-json
          (buffer-substring-no-properties
           (point-min) (point-max))))
    :enumerate-parser
    (let-alist it
      (let ((loc (cons (car (flymake-diag-region
                             flymake-collection-source
                             .location.row .location.column))
                       (cdr (flymake-diag-region
                             flymake-collection-source
                             .end_location.row .end_location.column)))))
        (list flymake-collection-source
              (car loc)
              (cdr loc)
              :warning
              (concat (when .code
                        (concat (propertize .code 'face 'flymake-collection-diag-id) " "))
                      .message))))))

;; ============================================================================
;; DOCUMENTATION & HELP
;; ============================================================================

(use-package devdocs
  :bind ("C-h D" . devdocs-lookup))

(use-package tldr
  :bind ("C-h t" . tldr))

(use-package lookup
  :ensure nil
  :bind (("C-c l" . jake/lookup-query))
  :load-path "lisp/"
  :init
  ;; Embark integration
  (with-eval-after-load 'embark
    (define-key embark-symbol-map (kbd "l") #'jake/lookup-query))
  :config
  (add-hook 'kill-emacs-hook #'jake/lookup-save-query-history))

(use-package ascii-art-to-unicode)

(use-package helpful
  :bind (;; Replace `async-shell-command' with `detached-shell-command'
         ([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key] . helpful-key)
         ([remap describe-command] . helpful-command)
         ([remap describe-symbol] . helpful-symbol)))

(use-package elisp-demos
  :after helpful
  :init
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))



;; ============================================================================
;; CASUAL - TRANSIENT MENU ENHANCEMENTS
;; ============================================================================
;; Casual provides user-friendly transient menus for various Emacs modes

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

(use-package casual-image
  :ensure nil
  :bind (:map image-mode-map ("C-o" . casual-image-tmenu)))

;; ============================================================================
;; ADDITIONAL DEVELOPMENT TOOLS
;; ============================================================================

(use-package eglot :ensure nil)

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

(use-package copilot-chat
  :bind (("C-c c c" . copilot-chat-display)))

(use-package restart-emacs)

(use-package detached
  :init
  (detached-init)
  (defun project-detached-compile ()
    "Run `compile' in the project root."
    (declare (interactive-only compile))
    (interactive)
    (let ((default-directory (project-root (project-current t)))
          (compilation-buffer-name-function
           (or project-compilation-buffer-name-function
               compilation-buffer-name-function)))
      (call-interactively #'detached-compile)))
  (defun project-detached-recompile (&optional edit-command)
    "Run `recompile' with appropriate buffer."
    (declare (interactive-only recompile))
    (interactive "P")
    (let ((compilation-buffer-name-function
           (or project-compilation-buffer-name-function
               ;; Should we error instead?  When there's no
               ;; project-specific naming, there is no point in using
               ;; this command.
               compilation-buffer-name-function)))
      (detached-compile-recompile edit-command)))
  :bind (;; Replace `async-shell-command' with `detached-shell-command'
         ([remap async-shell-command] . detached-shell-command)
         ;; Replace `compile' with `detached-compile'
         ([remap compile] . detached-compile)
         ([remap recompile] . detached-compile-recompile)
         ;; Replace built in completion of sessions with `consult'
         ([remap detached-open-session] . detached-consult-session)
         ([remap project-compile] . project-detached-compile)
         ([remap project-recompile] . project-detached-recompile))
  :custom ((detached-show-output-on-attach t)
           (detached-terminal-data-command system-type)
           (detached-filter-ansi-sequences t)))

;; ============================================================================
;; REMOTE FILE ACCESS - TRAMP
;; ============================================================================

(setopt remote-file-name-inhibit-locks t
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

(setopt magit-tramp-pipe-stty-settings 'pty)


(with-eval-after-load 'tramp
  (with-eval-after-load 'compile
    (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options)))

(with-eval-after-load 'compile
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter))

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(setopt bookmark-save-flag 1)

;; ============================================================================
;; LATEX & TYPESETTING
;; ============================================================================

(use-package tex
  :ensure auctex
  :hook ((LaTeX-mode . TeX-source-correlate-mode)
         (LaTeX-mode . visual-line-mode)
         (LaTeX-mode . flyspell-mode)
         (LaTeX-mode . TeX-PDF-mode)
         (LaTeX-mode . reftex-mode))
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-save-query nil)
  (TeX-master nil) ; query for master on first save
  (TeX-PDF-mode t)
  (LaTeX-item-indent 0)
  (LaTeX-command-style '(("" "%(latex)")))
  (TeX-engine-alist '((default
                       "Tectonic"
                       "tectonic -X compile -f plain %T"
                       "tectonic -X watch"
                       nil)))
  (TeX-source-correlate-start-server t)
  (TeX-process-asynchronous t)
  (TeX-check-TeX nil)
  (TeX-engine 'default)
  :config
  (with-eval-after-load 'reftex
    (setq reftex-plug-into-AUCTeX t
          reftex-cite-format 'default
          reftex-ref-style-default-list '("Default" "Varioref"))))

(use-package typst-ts-mode
  :vc (:url "https://codeberg.org/meow_king/typst-ts-mode")
  :bind (:map
         typst-ts-mode-map
         ("C-c C-c" . typst-ts-tmenu))
  :custom
  (typst-ts-watch-options '("--open"))
  (typst-ts-mode-grammar-location (expand-file-name "tree-sitter/libtree-sitter-typst.so" user-emacs-directory))
  (typst-ts-mode-enable-raw-blocks-highlight t))

;;; init.el ends here
