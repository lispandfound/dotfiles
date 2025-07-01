
;; Startup speed, annoyance suppression
(setq gc-cons-threshold 10000000)
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)

;; Silence stupid startup message
(setq inhibit-startup-echo-area-message (user-login-name))
;; must be set *before* use-package is loaded to be effective
(setq use-package-enable-imenu-support t)
(setq package-enable-at-startup nil)
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
