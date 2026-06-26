;;; config-completion.el --- Completion framework -*- lexical-binding: t; -*-

;;; =========================================================================
;;; ORDERLESS — flexible completion matching
;;; =========================================================================

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles orderless partial-completion))
                                   (eglot      (styles orderless))
                                   (eglot-capf (styles orderless))))
  (orderless-component-separator #'orderless-escapable-split-on-space)
  ;; Dispatch characters: prefix a word with one of these to change matching.
  ;;   ! without   & annotation   % char-fold   ` initialism
  ;;   = literal   ^ literal-prefix   ~ flex
  (orderless-affix-dispatch-alist
   '((?! . orderless-without-literal)
     (?& . orderless-annotation)
     (?% . char-fold-to-regexp)
     (?\` . orderless-initialism)
     (?= . orderless-literal)
     (?^ . orderless-literal-prefix)
     (?~ . orderless-flex)))
  :config
  ;; Don't override match highlighting in the first column.
  (set-face-attribute 'completions-first-difference nil :inherit nil))

;;; =========================================================================
;;; VERTICO — vertical minibuffer completion UI
;;; =========================================================================

(use-package vertico
  :custom
  (vertico-cycle t)
  (vertico-count 17)
  (vertico-resize nil)
  :init
  ;; Show [CRM<sep>] indicator when completing-read-multiple is active.
  (advice-add #'completing-read-multiple :filter-args
              (lambda (args)
                (cons (format "[CRM%s] %s"
                              (replace-regexp-in-string
                               "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                               crm-separator)
                              (car args))
                      (cdr args))))
  :config
  (vertico-mode 1)
  ;; Use consult for in-buffer completion popup when vertico is active.
  (setq-default completion-in-region-function
                (lambda (&rest args)
                  (apply (if vertico-mode
                             #'consult-completion-in-region
                           #'completion--in-region)
                         args)))
  ;; Enable vertico-repeat (C-S-r) to replay the last session.
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save))

(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind (:map vertico-map
         ("RET"   . vertico-directory-enter)
         ("DEL"   . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-multiform
  :ensure nil
  :after vertico
  :config
  (vertico-multiform-mode 1)
  ;; Highlight directory candidates.
  (defun my/vertico-highlight-directory (file)
    (when (string-suffix-p "/" file)
      (add-face-text-property 0 (length file) 'marginalia-file-priv-dir 'append file))
    file)
  (add-to-list 'vertico-multiform-categories
               '(file (+vertico-transform-functions . my/vertico-highlight-directory)))
  (setq vertico-multiform-commands
        '((consult-imenu   buffer)
          (consult-outline buffer)))
  (setq vertico-multiform-categories
        '((consult-grep buffer))))

;;; =========================================================================
;;; MARGINALIA — completion candidate annotations
;;; =========================================================================

(use-package marginalia
  ;; :demand t overrides the deferral that :bind would otherwise trigger,
  ;; ensuring marginalia-mode is enabled at startup rather than on first M-A.
  :demand t
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :config
  (marginalia-mode 1))

;;; =========================================================================
;;; NERD-ICONS-COMPLETION — icons in the minibuffer
;;; =========================================================================

(use-package nerd-icons-completion
  :after (marginalia nerd-icons)
  :config
  (nerd-icons-completion-mode 1)
  ;; Called here rather than via marginalia-mode-hook so it runs at the
  ;; correct time — marginalia-mode is already on when this package loads.
  (nerd-icons-completion-marginalia-setup))

;;; =========================================================================
;;; CONSULT — enhanced navigation and search
;;; =========================================================================

(use-package consult
  :bind
  (;; Remaps — these override the built-in commands wherever they are bound.
   ([remap switch-to-buffer]              . consult-buffer)
   ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
   ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame)
   ([remap bookmark-jump]                 . consult-bookmark)
   ([remap recentf-open-files]            . consult-recent-file)
   ([remap recentf-open]                  . consult-recent-file)
   ([remap yank-pop]                      . consult-yank-pop)
   ([remap goto-line]                     . consult-goto-line)
   ([remap imenu]                         . consult-imenu)
   ([remap Info-search]                   . consult-info)
   ([remap locate]                        . consult-locate)
   ([remap load-theme]                    . consult-theme)
   ;; Direct bindings
   ("C-x r b"  . consult-bookmark)
   ("M-g i"    . consult-imenu)
   ("M-g I"    . consult-imenu-multi)
   ("M-g o"    . consult-outline)
   ("M-g M-g"  . consult-goto-line)
   ("M-s r"    . consult-ripgrep)
   ("M-s l"    . consult-line)
   ("M-s L"    . consult-line-multi)
   ("M-s k"    . consult-keep-lines)
   ("M-s f"    . consult-find)
   ("C-c s"    . consult-ripgrep))
  :custom
  ;; Guard against nil default-directory in special buffers (e.g. *scratch*).
  (consult-project-function (lambda (may-prompt)
                               (ignore-errors
                                 (when-let ((proj (project-current may-prompt)))
                                   (project-root proj)))))
  (xref-show-xrefs-function       #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-narrow-key "<")
  (consult-line-numbers-widen t)
  ;; Async search tuning (matches Doom's values).
  (consult-async-min-input 2)
  (consult-async-refresh-delay  0.15)
  (consult-async-input-throttle 0.2)
  (consult-async-input-debounce 0.1)
  ;; Prefer fd over find when available.
  (consult-fd-args
   '((if (executable-find "fd" 'remote) "fd" "find")
     "--color=never" "--full-path" "--absolute-path"
     "--hidden" "--exclude" ".git"))
  :config
  ;; Manual preview for slow sources — press C-SPC to preview instead of auto.
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   consult-source-recent-file consult-source-project-recent-file
   consult-source-bookmark
   :preview-key "C-SPC")
  ;; Theme preview with debounce to avoid flickering.
  (consult-customize
   consult-theme :preview-key '("C-SPC" :debounce 0.5 any))
  ;; Ensure recentf is on before commands that need it.
  (advice-add #'consult-recent-file :before (lambda (&rest _) (recentf-mode 1)))
  (advice-add #'consult-buffer      :before (lambda (&rest _) (recentf-mode 1)))
  ;; Org buffer source for consult-buffer.
  (defvar my/consult-source-org-buffer
    (list :name     "Org Buffer"
          :category 'buffer
          :narrow   ?o
          :hidden   t
          :face     'consult-buffer
          :history  'buffer-name-history
          :state    #'consult--buffer-state
          :items
          (lambda ()
            (mapcar #'buffer-name
                    (seq-filter
                     (lambda (b)
                       (eq (buffer-local-value 'major-mode b) 'org-mode))
                     (buffer-list))))))
  (add-to-list 'consult-buffer-sources 'my/consult-source-org-buffer 'append))

;;; =========================================================================
;;; CONSULT-DIR — jump to directories in file prompts
;;; =========================================================================

(use-package consult-dir
  :bind (([remap list-directory] . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  :config
  ;; Add TRAMP SSH and local sources.
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-ssh t)
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-local t)
  ;; Docker container source (docker is installed).
  (defun my/consult-dir--docker-hosts ()
    (when (executable-find "docker")
      (cl-loop for line in (cdr (ignore-errors (process-lines "docker" "ps")))
               for parts = (split-string line "[[:space:]]+" t)
               collect (format "/docker:%s:/" (car (last parts))))))
  (defvar my/consult-dir-source-docker
    `(:name "Docker" :narrow ?d :category file
      :face consult-file :history file-name-history
      :items ,#'my/consult-dir--docker-hosts))
  (add-to-list 'consult-dir-sources 'my/consult-dir-source-docker t))

;;; =========================================================================
;;; CONSULT-YASNIPPET — pick snippets via consult
;;; =========================================================================

(use-package consult-yasnippet
  :after yasnippet
  :bind ([remap yas-insert-snippet] . consult-yasnippet))

;;; =========================================================================
;;; EMBARK — context-aware actions on candidates
;;; =========================================================================

(use-package embark
  :bind
  (("C-."   . embark-act)
   ("C-;"   . embark-dwim)
   ("C-h B" . embark-bindings)
   ([remap describe-bindings] . embark-bindings)
   :map minibuffer-local-map
   ("C-;"   . embark-act)
   ("C-c C-;" . embark-export)
   ("C-c C-l" . embark-collect))
  :custom
  (embark-quit-after-action nil)
  :init
  ;; Use embark for C-h on prefix keys (shows what's available).
  (setq which-key-use-C-h-commands nil
        prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode)
  :config
  ;; Open magit-status from embark on file candidates.
  (keymap-set embark-file-map "g" #'magit-status))

;;; =========================================================================
;;; CORFU — in-buffer completion UI
;;; =========================================================================

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-count 16)
  (corfu-max-width 120)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match t)
  (corfu-preselect 'prompt)
  :config
  (global-corfu-mode 1))

(use-package corfu-history
  :ensure nil
  :after corfu
  :config (corfu-history-mode 1))

(use-package corfu-popupinfo
  :ensure nil
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom (corfu-popupinfo-delay '(0.5 . 1.0)))

;;; =========================================================================
;;; CAPE — completion-at-point extensions
;;; =========================================================================

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

;;; =========================================================================
;;; WGREP — editable grep / consult-ripgrep results
;;; =========================================================================

(use-package wgrep
  :custom (wgrep-auto-save-buffer t))

(provide 'config-completion)
;;; config-completion.el ends here
