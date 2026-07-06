;;; early-init.el --- Early initialisation -*- lexical-binding: t; -*-

;; Disable package.el — elpaca takes over entirely.
(setq package-enable-at-startup nil)

;; Must be set early for imenu support to work
(setq use-package-enable-imenu-support t)

;; Runtime/generated files go here; gitignored so they don't pollute the repo.
(defvar my/local-dir
  (expand-file-name ".local/" user-emacs-directory))

;; Strip UI chrome before the first frame is drawn to avoid flicker.
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq frame-inhibit-implied-resize t)

;; Redirect native-comp cache out of the config directory.
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache (expand-file-name "eln-cache/" my/local-dir)))

;; Redirect the auto-save-list (session lock file prefix) early.
(setq auto-save-list-file-prefix
      (expand-file-name "auto-saves/saves-" my/local-dir))

;; Suppress noisy native-comp warnings.
(setq native-comp-async-report-warnings-errors 'silent)

;; Raise the GC threshold during startup so we don't GC on every allocation.
;; gcmh (see config-emacs.el) takes over gc-cons-threshold management once
;; loaded; file-name-handler-alist is restored here.
(defvar my/startup-file-name-handler file-name-handler-alist)

(setq gc-cons-threshold most-positive-fixnum
      file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist my/startup-file-name-handler)))
