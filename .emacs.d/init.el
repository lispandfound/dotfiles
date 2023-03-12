(defvar elpaca-installer-version 0.2)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(when-let ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
           (build (expand-file-name "elpaca/" elpaca-builds-directory))
           (order (cdr elpaca-order))
           ((add-to-list 'load-path (if (file-exists-p build) build repo)))
           ((not (file-exists-p repo))))
  (condition-case-unless-debug err
      (if-let ((buffer (pop-to-buffer-same-window "*elpaca-installer*"))
               ((zerop (call-process "git" nil buffer t "clone"
                                     (plist-get order :repo) repo)))
               (default-directory repo)
               ((zerop (call-process "git" nil buffer t "checkout"
                                     (or (plist-get order :ref) "--"))))
               (emacs (concat invocation-directory invocation-name))
               ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                     "--eval" "(byte-recompile-directory \".\" 0 'force)"))))
          (progn (require 'elpaca)
                 (elpaca-generate-autoloads "elpaca" repo)
                 (kill-buffer buffer))
        (error "%s" (with-current-buffer buffer (buffer-string))))
    ((error) (warn "%s" err) (delete-directory repo 'recursive))))
(require 'elpaca-autoloads)
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))




;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

(setq use-package-always-defer t)
;; Block until current queue processed.
(elpaca-wait)

(use-package esh-module
  :elpaca nil
  :custom
  (eshell-prefer-lisp-functions t)
  (eshell-prefer-lisp-variables t)
  (password-cache t)
  (password-cache-expiry 3600)
  :config
  (add-to-list 'eshell-modules-list 'eshell-tramp))

(use-package emacs
  :elpaca nil
  :custom
  (native-comp-async-report-warnings-errors nil)
  (vc-follow-symlinks t)
  (epg-pinentry-mode 'loopback)
  (initial-major-mode 'text-mode)
  (completion-cycle-threshold 3)
  (tab-always-indent 'complete)
  (ispell-program-name "hunspell")
  (ring-bell-function 'ignore)
  (save-interprogram-paste-before-kill t)
  (apropos-do-all t)
  (mouse-yank-at-point t)
  (require-final-newline t)
  (visible-bell t)
  (load-prefer-newer t)
  (backup-by-copying t)
  (backup-directory-alist '(("." . "~/.emacs.d/saves")))
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (version-control t)
  (frame-inhibit-implied-resize t)
  (dictionary-server "localhost")
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  (inhibit-splash-screen t)
  
  :config
  (load custom-file)
  (add-hook 'after-init-hook #'global-display-line-numbers-mode)
  
  (unless (memq window-system '(mac ns))
    (menu-bar-mode -1))
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
  (when (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))
  (fset 'yes-or-no-p 'y-or-n-p)
  (autoload 'zap-up-to-char "misc"
    "Kill up to, but not including ARGth occurrence of CHAR." t)

  ;; https://www.emacswiki.org/emacs/SavePlace
  (save-place-mode 1)
  (winner-mode)
  (global-set-key (kbd "M-/") 'hippie-expand)
  (global-set-key (kbd "M-z") 'zap-up-to-char)
  (global-set-key (kbd "s-t") 'eshell)
  (global-set-key (kbd "H-s-t") 'eshell)
  (global-set-key (kbd "s-/") 'winner-undo)
  (global-set-key (kbd "H-s-/") 'winner-undo)
  (global-set-key (kbd "s-?") 'winner-redo)
  (global-set-key (kbd "H-s-?") 'winner-redo)
  (global-set-key (kbd "M-o") 'other-window)
  (global-set-key (kbd "M-g c") 'compile)
  (global-set-key (kbd "M-g r") 'recompile)

  (global-set-key (kbd "C-s") 'isearch-forward-regexp)
  (global-set-key (kbd "C-r") 'isearch-backward-regexp)
  (global-set-key (kbd "C-M-s") 'isearch-forward)
  (global-set-key (kbd "C-M-r") 'isearch-backward)

  (show-paren-mode 1)
  (setq-default indent-tabs-mode nil)
  (savehist-mode 1)


  (unless backup-directory-alist
    (setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                                   "backups")))))
  (repeat-mode)
  (electric-pair-mode 1)
  (electric-indent-mode 1)

  (defun dvorak-translation ()
    (keyboard-translate ?\C-t ?\C-x)
    (keyboard-translate ?\C-x ?\C-t))
  (defun setup-frame-keyboard (&optional frame)
    "Re-map keys in the current terminal."
    (with-selected-frame (or frame (selected-frame))
      (dvorak-translation)))
  (dvorak-translation)
  (add-hook 'after-make-frame-functions #'setup-frame-keyboard)

  )

(use-package uniquify
  :elpaca nil
  :custom (uniquify-buffer-name-style 'forward)
  )


(use-package org
  :custom ((org-latex-pdf-process '("latexmk -f -pdf -shell-escape -%latex -interaction=nonstopmode -output-directory=%o %f"))
 	   (org-latex-compiler "lualatex")
           (org-latex-listings 'minted)
	   (org-stuck-projects '("+LEVEL=2+PROJECT" ("TODO") nil ""))
           (org-use-speed-commands t)
	   (org-highlight-latex-and-related '(script entities))
	   (org-agenda-files '("~/Sync/todo.org" "~/Sync/notes.org"))
	   (org-refile-targets '((nil . (:maxlevel . 2)) ("~/Sync/archive.org" . (:level . 1))))
	   (org-directory "~/Sync/")
           (org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "KILL(k)") (sequence "[ ](T)" "[?](W)" "[P](P)" "|" "[X](D)" "[-](K)" )))
           (org-pretty-entities t)
           (org-hide-emphasis-markers t)
           (org-superstar-headline-bullets-list '(" "))
           (org-attach-id-dir ".attach")
           (org-ellipsis "  ")
	   (org-agenda-block-separator ""))
  :init

  (setq +org-capture-todo-file "~/Sync/todo.org")
  (setq +org-capture-notes-file "~/Sync/notes.org")
  (setq org-capture-templates '(("t" "Personal todo" entry
                                 (file+headline +org-capture-todo-file "Inbox")
                                 "* TODO %?\n%i\n" :prepend t)
                                ("p" "Paper" entry
                                 (file+headline +org-capture-todo-file "Inbox")
                                 "* TODO Read %? :paper:\n" :prepend t)
                                ("n" "Personal notes" entry
                                 (file +org-capture-notes-file)
                                 "* %u %?\n%i\n" :prepend t)))
  (global-set-key (kbd "C-c a") #'org-agenda)
  (global-set-key (kbd "C-c n") #'org-capture)
  :config
  (require 'ox-beamer)
  (org-babel-do-load-languages 'org-babel-load-languages '((haskell . t)))
  (advice-add 'org-agenda-quit :before 'org-save-all-org-buffers)
  (add-hook 'org-capture-after-finalize-hook #'org-save-all-org-buffers)
  (add-hook 'org-agenda-finalize-hook #'org-save-all-org-buffers)
  (add-to-list 'org-latex-packages-alist '("" "minted")))

(use-package which-key
  :demand t
  :config
  (which-key-mode))

(use-package modus-themes
  :demand t
  :config
  (load-theme 'modus-operandi t))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package corfu
  :init
  (global-corfu-mode))

(use-package project
  :config
  (add-to-list 'project-switch-commands '(magit-project-status "Magit Status" "m")))

(use-package eglot
  :hook ((python-mode . eglot-ensure)
         (haskell-mode . eglot-ensure)
         (latex-mode . eglot-ensure))
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 256)
  (eglot-ignored-server-capabilites '(:documentHighlightProvider)))

;; Enable vertico
(use-package vertico
  :elpaca (vertico :files (:defaults "extensions/*"))
  :init (vertico-mode))


(use-package vertico-repeat
  :after vertico
  :elpaca nil
  :bind ("M-R" . vertico-repeat)
  :hook (minibuffer-setup . vertico-repeat-save))
;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :elpaca nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; A few more useful configurations...
(use-package emacs
  :elpaca nil
  :init
  (setq epg-gpg-program "gpg")
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
  (setq-default abbrev-mode t)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))


(use-package marginalia

  :config
  (marginalia-mode))

(use-package embark
  :bind*
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-," . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
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
(use-package citar
  :bind ("C-c b" . citar-open)
  :custom
  (citar-bibliography '("~/Sync/bibliography/bibliography.bib")
                      citar-library-paths '("~/Sync/bibliography/pdfs")))
(use-package citar-embark
  :after citar embark
  :elpaca nil
  :no-require
  :init (citar-embark-mode))


(use-package org-appear
  :custom ((org-appear-inside-latex t)
           (org-appear-autosubmarkers t))
  :hook (org-mode . org-appear-mode))


(use-package latex
  :elpaca auctex
  :init
  (setq TeX-auto-save t
	TeX-parse-self t
	TeX-electric-math (cons "\\(" "\\)")
	LaTeX-electric-left-right-brace t
	TeX-electric-sub-and-superscript t
	TeX-command-extra-options "-shell-escape"
	TeX-master nil
	TeX-engine 'xetex)
  :hook ((TeX-after-compilation-finished-functions . TeX-revert-document-buffer)
	 (LaTeX-mode . turn-on-auto-fill)
	 (LaTeX-mode . LaTeX-math-mode)
	 (LaTeX-mode . jake/rem-environments))
  :config
  (defun jake/theorem-environments ()
    (LaTeX-add-environments '("theorem"  LaTeX-env-label)
			    '("lemma" LaTeX-env-label)
			    '("definition" LaTeX-env-label)
			    '("corollary" LaTeX-env-label))
    (setf LaTeX-label-alist (cl-list* '("lemma" . "lem:") '("theorem" . "thm:") '("definition" . "def:") '("corollary" . "cor:") LaTeX-label-alist))))

(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )
(use-package cdlatex
  :hook (org-mode . org-cdlatex-mode)
  :init (defun add-labelled-env (environment shortcut)
	  (add-to-list 'cdlatex-env-alist (list environment (format "\\begin{%s}\nAUTOLABEL\n?\n\\end{%s}" environment environment) nil))
	  (add-to-list 'cdlatex-command-alist (list shortcut (format "Insert %s env" environment) "" 'cdlatex-environment (list environment) t nil)))
  (defun jake/cdlatex-hook ()
    
    (cdlatex-mode)
    ;; Fixing #35 on github, cdlatex-takeover-parenthesis doesn't work...
    (unbind-key "(" cdlatex-mode-map)
    (unbind-key "{" cdlatex-mode-map)
    (unbind-key "[" cdlatex-mode-map))
  
  (add-hook 'LaTeX-mode-hook 'jake/cdlatex-hook)
  
  :config
  (dolist (kv '(("theorem" "thm") ("definition" "def") ("corollary" "cor") ("lemma" "lem")))
    (add-labelled-env (car kv) (cadr kv))))

(use-package reftex
  :elpaca nil
  :after latex
  :hook ((LaTeX-mode . reftex-mode)))

(use-package auth-source
  :elpaca nil
  :custom (auth-sources '("~/.authinfo.gpg")))


(use-package elfeed
  :custom ((elfeed-feeds '(("ttrss+https://jake@jakefaulkner.me/tt-rss" :use-authinfo t)))))

(use-package elfeed-protocol
  :custom ((elfeed-protocol-enabled-protocols '(ttrss)))
  :demand t
  :after elfeed
  :config
  (elfeed-protocol-enable))

(use-package popper
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*helpful .*\\*$"
          "\\*Warnings\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "^\\*eshell.*\\*$" eshell-mode ;eshell as a popup
          "^\\*shell.*\\*$"  shell-mode  ;shell as a popup
          "^\\*term.*\\*$"   term-mode   ;term as a popup
          "^\\*vterm.*\\*$"  vterm-mode
          help-mode
          helpful-mode
          compilation-mode))

  (popper-mode +1)
  (popper-echo-mode +1))



(use-package apheleia
  :init (apheleia-global-mode 1))


;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
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

(use-package citar-embark
  :after citar embark
  :no-require
  :config (citar-embark-mode))

(use-package beacon

  :init (beacon-mode 1))

(use-package tempo
  :elpaca nil
  :custom (tempo-interactive t)
  :bind (("M-g M-e" . tempo-forward-mark)
	 ("M-g M-a" . tempo-backward-mark))
  :demand t
  :init
  (defun setup-tempo ()
    (defadvice tempo-define-template (after no-self-insert-in-abbrevs activate)
      "Skip self-insert if template function is called by an abbrev."
      (put (intern (concat "tempo-template-" (ad-get-arg 0))) 'no-self-insert t))
    (load (concat user-emacs-directory "tempo.el")))
  (add-hook 'after-init-hook #'setup-tempo))

(use-package haskell-mode
  :hook (haskell-mode . interactive-haskell-mode))

(use-package exec-path-from-shell
  :demand t
  :init (exec-path-from-shell-initialize))


(use-package treesit
  :elpaca nil
  :custom (treesit-font-lock-level 4))

(use-package treesit-auto
  :custom (treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode))


(use-package flymake
  :config
  (setq flymake-no-changes-timeout 3))


(use-package flyspell
  :elpaca nil
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))
(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h o" . helpful-symbol)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)))


(use-package mu4e
  :elpaca nil
  :load-path "/usr/share/emacs/site-lisp/mu4e"
  :commands mu4e mu4e-compose-new
  :config
  ;; set mail user agent to mu4e
  (setq mail-user-agent 'mu4e-user-agent)

  ;; set mu4e mail directory
  (setq mu4e-maildir "~/.mail")

  ;; use mu4e for sending mail
  (setq send-mail-function 'smtpmail-send-it
        message-send-mail-function 'smtpmail-send-it)

  ;; SMTP settings for sending mail
  (setq smtpmail-smtp-server "localhost"
        smtpmail-smtp-service 1025) ;; or the port number that DavMail is using

  ;; enable mu4e
  (setq mu4e-get-mail-command "mbsync -a"
        mu4e-update-interval 300 ;; update every 5 minutes
        mu4e-view-show-addresses t
        mu4e-compose-signature-auto-include nil
        mu4e-view-show-images t
        mu4e-headers-include-related nil
        mu4e-use-fancy-chars t
        mu4e-headers-date-format "%Y-%m-%d %H:%M"
        mu4e-headers-fields '((:human-date . 12)
                              (:flags . 4)
                              (:from . 22)
                              (:subject . nil))
        mu4e-headers-visible-columns 100
        mu4e-headers-skip-duplicates t
        mu4e-headers-sort-direction 'descending
        mu4e-headers-auto-update t
        mu4e-compose-dont-reply-to-self t
        mu4e-confirm-quit nil
        mu4e-compose-format-flowed t
        mu4e-compose-dont-reply-to-self t
        mu4e-view-prefer-html t)

  ;; specify the mail sources
  (setq mu4e-maildir-shortcuts
        '(("/inbox" . ?i)
          ("/sent" . ?s)
          ("/drafts" . ?d)
          ("/trash" . ?t)))

  ;; define the Exchange email account
  (setq mu4e-sent-folder "/sent"
        mu4e-drafts-folder "/drafts"
        mu4e-trash-folder "/trash"
        mu4e-refile-folder "/archive"
        mu4e-sent-messages-behavior 'delete
        mu4e-compose-signature-auto-include nil
        mu4e-user-mail-address-list '("jake.faulkner@pg.canterbury.ac.nz")
        user-mail-address "jake.faulkner@pg.canterbury.ac.nz"
        mu4e-sent-messages-behavior 'delete
        mu4e-maildir-shortcuts '(("/inbox" . ?i)
                                 ("/sent" . ?s)
                                 ("/drafts" . ?d)
                                 ("/trash" . ?t)))

  ;; use davmail to retrieve Exchange email
  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq sendmail-program "msmtp"))

(use-package ox-moderncv
  :after org
  :demand t
  :elpaca (:repo "https://gitlab.com/lafrenierejm/org-cv.git"))

