(setq display-line-numbers t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; In case ELPA is not working
;; (setq package-archives '(("gnu" . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/gnu/")
;;                          ("melpa" . "https://melpa.org/packages/")))


;; TODO: Remove after Emacs 30
(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)
(setq package-install-upgrade-built-in t) 

(load-theme 'modus-vivendi)
(use-package moody
  :ensure t
  :config
  (moody-replace-mode-line-front-space)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(keyboard-translate ?\C-t ?\C-x)
(keyboard-translate ?\C-x ?\C-t)


(use-package better-defaults
  :ensure t
  :config
  (ido-mode -1))

(add-hook 'after-init-hook #'electric-pair-mode)

(use-package corfu
  :ensure t
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
  :init
  (global-corfu-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))


;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :ensure t
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
  :ensure t
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
  :after vertico
  :config
  (keymap-set vertico-map "RET" #'vertico-directory-enter)
  (keymap-set vertico-map "DEL" #'vertico-directory-delete-char)
  (keymap-set vertico-map "M-DEL" #'vertico-directory-delete-word)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))


;; A few more useful configurations...
(use-package emacs
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

  ;; Support opening new minibuffers from inside existing minibuffers.
  (setq enable-recursive-minibuffers t)

  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.
  (setq read-extended-command-predicate #'command-completion-default-include-p))


;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  ;; (setq completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)

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
  :ensure t
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-elisp-symbol)
         ("C-c p e" . cape-elisp-block)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p :" . cape-emoji)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
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
  :ensure t

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
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


(use-package treesit-auto
  :ensure t
  :config
  (global-treesit-auto-mode))

(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status))

(use-package forge
  :ensure t
  :bind ("C-c '" . forge-dispatch))

(use-package pyvenv
  :ensure t
  :commands pyvenv-activate)

(use-package numpydoc
  :ensure t
  :bind ("C-c M-n" . numpydoc-generate)
  :config
  (setq numpydoc-insert-examples-block nil))


(use-package skempo
  :vc (:fetcher github :repo xFA25E/skempo)
  :config
  (load (concat user-emacs-directory "skempo/python.el")))

;; Enable only if you want def/if/class to auto-expand
;; (setq python-skeleton-autoinsert t)

(global-set-key (kbd "C-c C-n") #'tempo-forward-mark)
(global-set-key (kbd "C-c C-p") #'tempo-backward-mark)

(setq-default abbrev-mode t)

(setq eglot-report-progress nil)

(use-package jinx
  :ensure t
  :hook (emacs-startup . global-jinx-mode)
  :bind (([remap ispell-word] . jinx-correct)
         ("C-M-$" . jinx-languages)))

(use-package ibuffer-project
  :ensure t
  :after ibuffer
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups)))))

(use-package which-key
  :ensure t
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
      dired-listing-switches "-alFh")
(setq tramp-use-ssh-controlmaster-options nil)

(use-package yaml-mode
  :ensure t)

(use-package csv-mode
  :ensure t
  :hook (csv-mode . csv-align-mode))

(setq vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)" vc-ignore-dir-regexp tramp-file-name-regexp))
(setq use-short-answers t)

(use-package haskell-mode
  :ensure t)

(setq display-buffer-alist '(("\\`.*e?shell\\*" (display-buffer-in-side-window (side . bottom)))))

(use-package dumb-jump
  :ensure t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(delete-selection-mode)


(use-package crux
  :ensure t
  :bind (("C-k" . crux-smart-kill-line)
         ("C-x 4 t" . crux-transpose-windows)
         ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
         ("C-c S" . crux-find-user-init-file)))

(use-package titlecase
  :ensure t
  :bind (("C-c M-c" . titlecase-dwim)))


(use-package casual-isearch
  :ensure t
  :bind (:map isearch-mode-map
              ("C-o" . casual-isearch-tmenu)))

(use-package elm-mode
  :ensure t)


(use-package casual-calc
  :ensure t
  :bind (:map calc-mode-map ("C-o" . #'casual-calc-tmenu)))

(use-package casual-dired
  :ensure t
  :bind (:map dired-mode-map ("C-o" . #'casual-dired-tmenu)))

(use-package casual-info
  :ensure t
  :bind (:map Info-mode-map ("C-o" . #'casual-info-tmenu)))

(use-package eglot-booster
  :vc (:fetcher github :repo jdtsmith/eglot-booster)
  :after eglot
  :config (eglot-booster-mode))

(use-package wgrep
  :ensure t)

(setq org-agenda-files '("~/todo.org")
      org-directory "~/"
      auth-sources '("~/.authinfo")
      auto-revert-avoid-polling t
      auto-revert-check-vc-info t
      auto-revert-interval 5
      sentence-end-double-space nil
      column-number-mode t
      initial-major-mode 'fundamental-mode
      x-underline-at-descent-line nil
      inhibit-splash-screen t)
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

(transient-define-argument pandoc-input-formats ()
  "Pandoc Input Format"
  :argument "--from="
  :class 'transient-option
  :choices
  '("bibtex"
    "biblatex"
    "bits"
    "commonmark"
    "commonmark_x"
    "creole"
    "csljson"
    "csv"
    "tsv"
    "djot"
    "docbook"
    "docx"
    "dokuwiki"
    "endnotexml"
    "epub"
    "fb2"
    "gfm"
    "haddock"
    "html"
    "ipynb"
    "jats"
    "jira"
    "json"
    "latex"
    "markdown"
    "markdown_mmd"
    "markdown_phpextra"
    "markdown_strict"
    "mediawiki"
    "man"
    "muse"
    "native"
    "odt"
    "opml"
    "org"
    "ris"
    "rtf"
    "rst"
    "t2t"
    "textile"
    "tikiwiki"
    "twiki"
    "typst"
    "vimwiki"))

(transient-define-argument pandoc-output-formats ()
  "Pandoc Output Format"
  :argument "--to="
  :class 'transient-option
  :choices '("asciidoc"
             "asciidoc_legacy"
             "asciidoctor"
             "beamer"
             "bibtex"
             "biblatex"
             "chunkedhtml"
             "commonmark"
             "commonmark_x"
             "context"
             "csljson"
             "djot"
             "docbook or docbook4"
             "docbook5"
             "docx"
             "dokuwiki"
             "epub or epub3"
             "epub2"
             "fb2"
             "gfm"
             "haddock"
             "html or html5"
             "html4"
             "icml"
             "ipynb"
             "jats_archiving"
             "jats_articleauthoring"
             "jats_publishing"
             "jats"
             "jira"
             "json"
             "latex"
             "man"
             "markdown"
             "markdown_mmd"
             "markdown_phpextra"
             "markdown_strict"
             "markua"
             "mediawiki"
             "ms"
             "muse"
             "native"
             "odt"
             "opml"
             "opendocument"
             "org"
             "pdf"
             "plain"
             "pptx"
             "rst"
             "rtf"
             "texinfo"
             "textile"
             "slideous"
             "slidy"
             "dzslides"
             "revealjs"
             "s5"
             "tei"
             "typst"
             "xwiki"
             "zimwiki"))


(transient-define-argument pandoc-output-file ()
  :argument "--output="
  :class 'transient-files
  :prompt "Output file: ")


(transient-define-prefix pandoc-interface ()
  ["Pandoc Options"
   ("-s" "Standalone file generation (i.e. with <head> or LaTeX preamble)" "-s")
   ("--dpi" "Output DPI" "--dpi=" :prompt "Output DPI: ")]
  ["Input/Output"
   ("-f" "--from=" pandoc-input-formats)
   ("-t" "--to=" pandoc-output-formats)
   ("-o" "--output=" pandoc-output-file)]
  ["File"
   ("<return>" "Convert this file" pandoc-convert-this-file :transient nil)
   ])

(transient-define-suffix pandoc-convert-this-file (the-prefix-arg)
  "Convert this file, using pandoc"
  :transient 'transient--do-call
  (interactive "P")
  (let ((args (transient-args (oref transient-current-prefix command))))
    (async-shell-command (s-concat "pandoc " (s-join " " (cons (buffer-file-name) args))))))





(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-vc-selected-packages
   '((vc-use-package :vc-backend Git :url "https://github.com/slotThe/vc-use-package"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
