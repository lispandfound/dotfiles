;;; init.el --- Emacs configuration entry point -*- lexical-binding: t; -*-

;;; =========================================================================
;;; 1. ELPACA BOOTSTRAP
;;; =========================================================================

(defvar elpaca-installer-version 0.12)
;; Keep all elpaca data in .local/ to avoid polluting the config directory.
(defvar elpaca-directory      (expand-file-name "elpaca/"  my/local-dir))
(defvar elpaca-builds-directory  (expand-file-name "builds/"  elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca-activate)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
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

;; Install use-package integration and wait for it to be ready.
(elpaca elpaca-use-package (elpaca-use-package-mode))
(elpaca-wait)

;;; =========================================================================
;;; 2. GLOBAL USE-PACKAGE DEFAULTS
;;; =========================================================================

;; All use-package forms install via elpaca unless :ensure nil is set.
(setq use-package-always-ensure t)

;; Install compat first — many packages depend on it and will warn if loaded
;; outside elpaca's control.
(use-package compat :ensure t)

;; Install transient from MELPA before magit and gptel, which both require
;; a version newer than what ships with Emacs (>= 0.13).
(use-package transient :ensure t)

(elpaca-wait)

;;; =========================================================================
;;; 3. CORE EMACS SETTINGS
;;; =========================================================================

(use-package emacs
  :ensure nil
  :custom
  ;; Encoding
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  ;; Startup
  (inhibit-startup-message t)
  (initial-scratch-message nil)
  ;; Custom file — keep Emacs from polluting init.el
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  ;; Backup / auto-save go to .local/ so they don't appear in the repo.
  (backup-directory-alist `(("." . ,(expand-file-name "backup/" my/local-dir))))
  (auto-save-file-name-transforms `((".*" ,(expand-file-name "backup/" my/local-dir) t)))
  (backup-by-copying t)
  (delete-old-versions t)
  (kept-new-versions 6)
  ;; State files
  (recentf-save-file   (expand-file-name "recentf"  my/local-dir))
  (save-place-file     (expand-file-name "places"   my/local-dir))
  (savehist-file       (expand-file-name "savehist" my/local-dir))
  ;; Interaction
  (use-short-answers t)           ; y/n instead of yes/no
  (confirm-kill-processes nil)
  (ring-bell-function 'ignore)    ; no audio bell, ever
  (enable-recursive-minibuffers t) ; M-x inside find-file, etc.
  (mouse-yank-at-point t)         ; middle-click pastes at point, not cursor
  (sentence-end-double-space nil) ; M-e / fill-paragraph with single space
  ;; Display
  (display-line-numbers-type t)
  (column-number-mode t)
  (show-paren-context-when-offscreen t) ; show matching paren in echo area
  (truncate-string-ellipsis "…")        ; unicode ellipsis
  ;; Scrolling
  (scroll-conservatively 101)
  (scroll-margin 3)
  (auto-window-vscroll nil)       ; no jitter past tall lines/images
  (fast-but-imprecise-scrolling t) ; skip fontification mid-scroll
  ;; Editing
  (tab-width 4)
  (indent-tabs-mode nil)
  (fill-column 80)
  ;; Recent files
  (recentf-max-saved-items 200)
  (recentf-max-menu-items 20)
  ;; Auto-save visited files (real files, not crash-recovery files)
  (auto-save-visited-interval 30)
  :config
  (set-charset-priority 'unicode)
  (prefer-coding-system 'utf-8)
  ;; Global minor modes
  (electric-pair-mode 1)
  (show-paren-mode 1)
  (global-auto-revert-mode 1)
  (save-place-mode 1)
  (savehist-mode 1)
  (recentf-mode 1)
  (global-display-line-numbers-mode 1)
  ;; Pixel-level smooth scrolling (trackpad-friendly, Emacs 29+).
  (pixel-scroll-precision-mode 1)
  ;; Gracefully handle files with extremely long lines.
  (global-so-long-mode 1)
  ;; Auto-save visited files in place after idle (complements backup auto-save).
  (auto-save-visited-mode 1)
  ;; Show nesting depth when minibuffer is called recursively.
  (minibuffer-depth-indicate-mode 1)
  ;; Keep the cursor out of the read-only minibuffer prompt.
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  ;; Ensure .local/ subdirectories exist.
  (dolist (dir (list my/local-dir
                     (expand-file-name "backup/"     my/local-dir)
                     (expand-file-name "auto-saves/" my/local-dir)))
    (make-directory dir t)))

;;; =========================================================================
;;; 4. LOAD CONFIG MODULES
;;; =========================================================================

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Load order matters: keys first (keyboard translations must be in effect
;; before any package tries to bind keys), then UI, then tools, then lang.
(require 'config-keys)
(require 'config-ui)
(require 'config-windows)
(require 'config-completion)
(require 'config-editor)
(require 'config-emacs)
(require 'config-checkers)
(require 'config-tools)

;; Language modules
(require 'lang-elisp)
(require 'lang-cc)
(require 'lang-fortran)
(require 'lang-haskell)
(require 'lang-just)
(require 'lang-json)
(require 'lang-latex)
(require 'lang-markdown)
(require 'lang-org)
(require 'lang-python)
(require 'lang-rust)
(require 'lang-sh)
(require 'lang-yaml)

;;; =========================================================================
;;; 5. SERVER & CUSTOM FILE
;;; =========================================================================

(require 'server)
(unless (server-running-p)
  (server-start))

(when (file-exists-p custom-file)
  (load custom-file))
