;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;;; Commentary:
;; Early initialization settings for Emacs 29+

;;; Code:

;; Startup speed, annoyance suppression
(setq gc-cons-threshold 10000000)
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)

;; Silence startup messages
(setq inhibit-startup-echo-area-message (user-login-name))
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")

;; Package system - use native use-package (Emacs 29+)
(setq package-enable-at-startup nil)
(setq use-package-enable-imenu-support t)

;; GUI settings - must be set early to avoid flashing
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Line numbers - enable by default
(setq-default display-line-numbers t)

;;; early-init.el ends here
