;;; config-env.el --- Per-project environments via direnv -*- lexical-binding: t; -*-

;;; =========================================================================
;;; ENVRC — buffer-local direnv environments
;;; =========================================================================

;; Projects describe their toolchain in .envrc (usually `use flake'), as the
;; paper-template repos do: typst, uv, Python, R, plus variables such as
;; TYPST_PACKAGE_PATH, TYPST_FONT_PATHS, UV_PYTHON and LD_LIBRARY_PATH.
;; envrc runs `direnv export' for each buffer's directory and makes
;; `process-environment' and `exec-path' buffer-local, so every process a
;; buffer starts (eglot servers, apheleia formatters, compile, run-python,
;; flymake backends, magit's git hooks) sees that project's tools and
;; nothing leaks between projects.  Outside a direnv tree buffers keep the
;; global environment from exec-path-from-shell.
;;
;; Note: Nix-backed .envrc files are slow to evaluate the first time; after
;; editing flake.nix or .envrc, `C-c e r' (envrc-reload) refreshes every
;; buffer in that tree.  A directory whose .envrc has not been allowed shows
;; a warning-coloured lighter; `C-c e a' allows it.

(use-package inheritenv
  :demand t)

(use-package envrc
  :demand t
  :after inheritenv
  :bind-keymap ("C-c e" . envrc-command-map)
  :config
  ;; Commands that start their process from a buffer other than the one the
  ;; user is in -- a temp buffer or a freshly created comint buffer -- would
  ;; otherwise see the global environment, because the buffer-local values
  ;; don't follow them there.  This advice rebinds the defaults to the
  ;; calling buffer's values for the duration of the call.
  ;;   python-shell-make-comint  run-python spawns from the new *Python* buffer
  ;;   org-babel-eval            babel shell/python blocks use a temp buffer
  ;;   shell-command-to-string   helpers that shell out via a temp buffer
  (dolist (fn '(python-shell-make-comint
                org-babel-eval
                shell-command-to-string))
    (advice-add fn :around #'envrc-propagate-environment)))

;; The global mode is switched on last, once every other package has been
;; configured: `envrc-mode' then runs ahead of the other functions on
;; `after-change-major-mode-hook', so later hooks already see the project
;; environment.  eglot-ensure defers its connection to `post-command-hook',
;; by which point envrc has applied.
(add-hook 'elpaca-after-init-hook #'envrc-global-mode)

(provide 'config-env)
;;; config-env.el ends here
