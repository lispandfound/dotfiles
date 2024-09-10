;;; -*- lexical-binding: t -*-

(require 'package)
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq-default display-line-numbers t)

(setq use-package-always-defer t)
(setq use-package-always-ensure t)


(set-frame-font "Jetbrains Mono-10")

(use-package transient
  :demand t)

(use-package doom-themes
  :demand t
  :config (load-theme 'doom-one t))

(use-package moody
  :demand t
  :config
  (moody-replace-mode-line-front-space)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(keyboard-translate ?\C-t ?\C-x)
(keyboard-translate ?\C-x ?\C-t)

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

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

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

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )

(use-package better-defaults
  :demand t
  :config
  (ido-mode -1))

(add-hook 'after-init-hook #'electric-pair-mode)

(use-package corfu
  ;; Optional customizations
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto nil)                 ;; Enable auto completion
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

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


(use-package treesit-auto
  :demand t
  :config
  (global-treesit-auto-mode))



(use-package magit
  :bind ("C-c g" . magit-status)
  :init (with-eval-after-load 'project
          (add-to-list 'project-switch-commands '(magit-project-status "Magit" "m"))))

(use-package forge
  :bind ("C-c '" . forge-dispatch))

(use-package browse-at-remote
  :bind ("C-c C-o" . browse-at-remote))


(use-package pyvenv)

(use-package python
  :bind (:map python-ts-mode-map
              ("M-n" . insert-numpydoc))

  :config
  (add-hook 'python-ts-mode-hook (lambda ()
                                   (setq-local transpose-sexps-function #'treesit-transpose-sexps
                                               devdocs-current-docs '("pandas~2" "numpy~1.23" "python~3.12"))))
  (require 's)
  (require 'dash)
  (defun python-get-treesit-def ()
    (treesit-parent-until (treesit-node-at (point)) (lambda (node) (or (s-equals? (treesit-node-type node) "function_definition")
                                                                       (s-equals? (treesit-node-type node) "class_definition")))))

  (defun python-parameters (node)
    (if-let ((parameters (treesit-node-child-by-field-name node "parameters")))
        (-map (lambda (node) (cons (python-parameter-name node) (python-parameter-type node)))
              (-filter #'python-parameter-name (treesit-node-children  parameters t)))))

  (defun python-function-is-method-p (node)
    (if-let ((grandparent (treesit-node-parent (treesit-node-parent node))))
        (s-equals? (treesit-node-type grandparent) "class_definition")))

  (defun python-parameter-name (node)
    (if-let ((parameter-name (treesit-node-text (treesit-search-subtree node "identifier"))))
        (substring-no-properties parameter-name)))
  (defun python-annotated-type-extraction (node)
    (alist-get 't (treesit-query-capture node '(((type (generic_type (identifier ) @gener (type_parameter (type (identifier) @t) _)))
                                                 (:match "Annotated" @gener))))))

  (defun python-parameter-type (node)
    (if-let ((parameter-type (treesit-node-text (treesit-node-child-by-field-name node "type"))))
        (let ((unannotated-type (python-annotated-type-extraction (treesit-node-child-by-field-name node "type"))))
          (s-replace-regexp "[[:space:]\n]+" " " (substring-no-properties (if unannotated-type (treesit-node-text unannotated-type) parameter-type))))))

  (defun python-return-type (node)
    (if-let ((return (treesit-node-text (treesit-node-child-by-field-name node "return_type"))))
        (s-replace-regexp "[[:space:]\n]+" " " (substring-no-properties return))))

  (defun python-extract-docstring (node)
    (alist-get 'c (treesit-query-capture (python-get-treesit-def) '((function_definition) body: (block :anchor (expression_statement (string) @c))))))

  (defun python-raises (node)
    (-map (lambda (node) (treesit-node-text (cdr node))) (treesit-query-capture node '((raise_statement (call function: (_) @e))))))

  (defun insert-numpydoc ()
    (interactive)
    (save-excursion
      (let* ((function-definition (python-get-treesit-def))
             (parameters (-filter (lambda (param) (or (not (python-function-is-method-p function-definition))
                                                      (and
                                                       (not (s-equals? (car param) "self"))
                                                       (not (s-equals? (car param) "cls")))))
                                  (python-parameters function-definition)))
             (return (python-return-type function-definition))
             (raises (python-raises function-definition))
             (existing-doc (python-extract-docstring function-definition)))
        (when (and existing-doc (yes-or-no-p "Overwrite Existing Docstring "))
          (delete-region  (treesit-node-start existing-doc) (treesit-node-end existing-doc)))
        (goto-char (treesit-node-start (treesit-node-child-by-field-name function-definition "body")))
        (insert "\"\"\"")
        (newline-and-indent)
        (insert "\"\"\"")
        (newline-and-indent)
        (previous-line 2)
        (end-of-line)
        (let ((description (s-trim (read-from-minibuffer "Short Description: "))))
          (insert description)
          (unless (s-equals? (substring description -1) ".")
            (insert ".")))
        (newline-and-indent 2)
        (insert (read-from-minibuffer "Long Description: "))
        (fill-paragraph)
        (when parameters
          (newline-and-indent 2)
          (insert "Parameters")
          (newline-and-indent)
          (insert "----------")
          (newline-and-indent)
          (dolist (parameter parameters)
            (let ((name (car parameter))
                  (type (cdr parameter)))
              (insert name)
              (when type
                (insert " : ")
                (insert type)
                )
              (newline-and-indent)
              (insert (read-from-minibuffer (s-concat "Description for " name ": ")))

              (call-interactively #'python-indent-shift-right))
            (newline-and-indent)))
        (when (and return (not (s-equals? return "None")))
          (newline-and-indent 2)
          (insert "Returns")
          (newline-and-indent)
          (insert "-------")
          (newline-and-indent)
          (insert return)
          (newline-and-indent)
          (insert (read-from-minibuffer "Description of return value: "))
          (call-interactively #'python-indent-shift-right))
        (when raises
          (newline-and-indent 2)
          (insert "Raises")
          (newline-and-indent)
          (insert "------")
          (newline-and-indent)
          (dolist (exception raises)
            (insert exception)
            (newline-and-indent)
            (insert (read-from-minibuffer (s-concat "Description for exception " exception ": ")))
            (call-interactively #'python-indent-shift-right))
          (newline-and-indent))))))

(use-package auto-virtualenv
  :hook (python-ts-mode . auto-virtualenv-set-virtualenv))

(add-hook 'python-ts-mode-hook #'eglot-ensure)


(global-set-key [remap newline] #'newline-and-indent)

(use-package skempo
  :demand t
  :vc (:url "https://github.com/xFA25E/skempo")
  :config
  (global-set-key (kbd "C-c C-n") #'tempo-forward-mark)
  (global-set-key (kbd "C-c C-p") #'tempo-backward-mark)
  (load (concat user-emacs-directory "skempo/python.el"))
  (load (concat user-emacs-directory "skempo/markdown.el")))

(use-package ws-butler
  :hook (prog-mode markdown-mode org-mode))

(setq-default abbrev-mode t)

(use-package eglot
  :custom
  (eglot-report-progress nil))

(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :bind (([remap ispell-word] . jinx-correct)
         ("C-M-$" . jinx-languages)))

(use-package ibuffer-project
  :custom (ibuffer-use-other-window t)
  :init
  (add-hook 'ibuffer-hook
            (lambda ()
              (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
              (unless (eq ibuffer-sorting-mode 'project-file-relative)
                (ibuffer-do-sort-by-project-file-relative)))))

(use-package project
  :bind ("C-x p t" . project-test)
  :init
  (defcustom project-test-command "just test" "Default test command for `project-test'")
  (defun project-test ()
    (interactive)
    (let ((compile-command project-test-command))
      (call-interactively #'project-compile))))

(use-package which-key
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
      isearch-lazy-count t)

(setq tramp-use-ssh-controlmaster-options nil)

(use-package yaml-mode)

(use-package csv-mode
  :hook (csv-mode . csv-align-mode))

(setq vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)" vc-ignore-dir-regexp tramp-file-name-regexp))
(setq use-short-answers t)

(use-package haskell-mode)

(add-to-list 'display-buffer-alist '("\\`.*-e?shell\\*" (display-buffer-in-side-window (side . bottom))))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package dumb-jump
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(delete-selection-mode)


(use-package crux
  :bind (("C-k" . crux-smart-kill-line)
         ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
         ("C-c S" . crux-find-user-init-file)
         ("<M-return>" . crux-smart-open-line)
         ("<M-S-return>" . crux-smart-open-line-above)))

(use-package titlecase
  :bind (("C-c M-c" . titlecase-dwim)))

(use-package casual-suite
  :bind*
  (
   ("M-g" . #'my/custom-avy-tmenu)
   :map calc-mode-map
   ("C-o" . #'casual-calc-tmenu)
   :map dired-mode-map
   ("C-o" . #'casual-dired-tmenu)
   :map isearch-mode-map
   ("C-o" . #'casual-isearch-tmenu)
   :map ibuffer-mode-map
   ("C-o" . #'casual-ibuffer-tmenu)
   ("F" . #'casual-ibuffer-filter-tmenu)
   ("s" . #'casual-ibuffer-sortby-tmenu)
   :map Info-mode-map
   ("C-o" . #'casual-info-tmenu)
   :map reb-mode-map
   ("C-o" . #'casual-re-builder-tmenu)
   :map reb-lisp-mode-map
   ("C-o" . #'casual-re-builder-tmenu)
   :map bookmark-bmenu-mode-map
   ("C-o" . #'casual-bookmarks-tmenu)
   :map org-agenda-mode-map
   ("C-o" . #'casual-agenda-tmenu))
  :init
  (defun my/custom-avy-tmenu ()
    (interactive)
    (require 'casual-avy)
    (transient-append-suffix 'casual-avy-tmenu "M-n"  '("E" "Error" consult-compile-error))
    (transient-append-suffix 'casual-avy-tmenu "E"  '("f" "Flymake Error" consult-flymake))
    (transient-append-suffix 'casual-avy-tmenu "p"  '("o" "Outline Item" consult-outline))
    (transient-append-suffix 'casual-avy-tmenu "o"  '("i" "Imenu Item" consult-imenu))
    (casual-avy-tmenu)))


(use-package elm-mode)

(use-package wgrep)

(use-package org
  :bind (("C-c a" . 'org-agenda)
         ("C-c x" . 'transient-org-capture))
  :custom
  (org-agenda-files '("~/org/todo.org"))
  (org-default-notes-file "~/org/todo.org")
  (org-directory "~/org")
  (org-todo-keywords '((sequence "TODO" "WAIT(w@/!)" "|" "DONE" "KILL")))
  :config
  (transient-define-suffix org-transient-capture--task (arg)
    (interactive "P")
    (org-capture arg "t"))
  (transient-define-suffix org-transient-capture--note (arg)
    (interactive "P")
    (org-capture arg "n"))
  (transient-define-prefix transient-org-capture ()
    "Org capture with transient."
    ["Template"
     ("t" "Task" org-transient-capture--task)
     ("n" "Note" org-transient-capture--note)]))


(use-package org-menu
  :vc (:url "https://github.com/sheijk/org-menu")
  :after org
  :bind (:map org-mode-map
              ("C-o" . 'org-menu)))


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
  :vc (:url "https://github.com/lispandfound/pandoc-transient")
  :bind (("C-c P" . pandoc-convert-transient)))


(use-package apheleia
  :config
  (apheleia-global-mode +1)
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
        '(ruff ruff-isort)))

(defun clone-buffer-other-window ()
  (interactive)
  (switch-to-buffer-other-window (current-buffer)))

(global-set-key (kbd "C-c b") #'clone-buffer-other-window)
(add-hook 'write-file-hooks 'delete-trailing-whitespace nil t)

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


(use-package avy
  :bind (("C-'" . avy-goto-word-0)
         ("C-c C-j" . avy-resume))
  :custom (avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)))

(use-package ascii-art-to-unicode)

(repeat-mode)

(defun hl-todo-and-notes ()
  (font-lock-add-keywords nil'(("\\<\\(TODO\\|NOTE\\):" 1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook #'hl-todo-and-notes)

(use-package edit-indirect)

(use-package mermaid-mode)

(use-package markdown-mode
  :mode ("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . gfm-mode))

(use-package visual-fill-column
  :hook (gfm-mode))

(use-package cylc-mode
  :ensure nil
  :commands (cylc-mode)
  :load-path "lisp/")



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
