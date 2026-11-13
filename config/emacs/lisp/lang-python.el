;;; lang-python.el --- Python configuration -*- lexical-binding: t; -*-

;; python-ts-mode is built-in (Emacs 29+); treesit-auto handles remapping.

(defcustom my/python-scratchpad-dir "~/src/scratchpad/"
  "uv project to run the Python REPL in when the current buffer has no
project-local uv environment (see `my/python-uv-project-root')."
  :type 'directory
  :group 'python)

(defun my/python-venv-project ()
  "Return the nearest directory above this buffer holding a uv `.venv'.
Searches from the visited file rather than `default-directory', which
envrc temporarily rebinds to the .envrc directory while it applies an
environment (see `my/python--venv-after-envrc')."
  (locate-dominating-file (or buffer-file-name default-directory) ".venv"))

(defun my/python-uv-project-root ()
  "Return the uv project directory to use for this buffer's REPL.
Uses the buffer's own uv project when one with a `.venv' encloses it,
otherwise falls back to `my/python-scratchpad-dir'."
  (directory-file-name
   (expand-file-name (or (my/python-venv-project) my/python-scratchpad-dir))))

(defun my/python-venv-activate ()
  "Activate the enclosing uv project's `.venv' for this buffer only.
Sets VIRTUAL_ENV and puts the venv's bin/ first on PATH and
`exec-path', buffer-locally and on top of whatever environment envrc
applied, so a project's direnv toolchain and its venv compose and
nothing leaks into other buffers.  VIRTUAL_ENV is what lets ty (via
rass) find the venv when it lives below the eglot project root, as in
a monorepo's analysis/ directory."
  (when-let* ((project (my/python-venv-project))
              (venv (directory-file-name (expand-file-name ".venv" project)))
              (bin (expand-file-name "bin" venv))
              ((file-directory-p bin))
              ((not (equal (getenv "VIRTUAL_ENV") venv))))
    ;; Build the new value under a `let' (setenv works on whatever is bound),
    ;; then install it buffer-locally once the `let' has unwound -- setting it
    ;; inside would only assign the temporary binding.
    (let ((env (let ((process-environment (copy-sequence process-environment)))
                 (setenv "VIRTUAL_ENV" venv)
                 (setenv "PATH" (concat bin path-separator (getenv "PATH")))
                 process-environment)))
      (setq-local process-environment env))
    (setq-local exec-path (cons bin exec-path))))

;; envrc resets `process-environment' and `exec-path' whenever it (re)applies
;; a direnv environment -- once just after the major-mode hooks run, and again
;; on `envrc-reload' -- so the venv is layered back on top each time.
(defun my/python--venv-after-envrc (buf &rest _)
  "Re-activate the uv venv in BUF after envrc has applied its environment."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (derived-mode-p 'python-base-mode)
        (my/python-venv-activate)))))

(with-eval-after-load 'envrc
  (advice-add 'envrc--apply :after #'my/python--venv-after-envrc))

(defun my/python-shell-setup ()
  "Point `run-python' at `uv run', preferring ipython.
Runs inside the buffer's own uv project when one is active, or
`my/python-scratchpad-dir' otherwise (see `my/python-uv-project-root').
IPython is pulled in via `--with' so it augments the project's
environment without being added to its own dependencies."
  (let ((root (my/python-uv-project-root)))
    (setq-local python-shell-interpreter "uv")
    (setq-local python-shell-interpreter-args
                (combine-and-quote-strings
                 (list "run" "--project" root
                       "--with" "ipython" "ipython" "--simple-prompt")))
    ;; `uv -i' can't be probed the way `python-shell-prompt-detect' expects,
    ;; and ipython's --simple-prompt output already matches python.el's own
    ;; default ">>> "/"... " regexps, so detection is unnecessary here and
    ;; would only produce a spurious warning.
    (setq-local python-shell-prompt-detect-enabled nil)))

(use-package python
  :ensure nil
  :hook
  ((python-base-mode . my/python-venv-activate)
   (python-ts-mode . eglot-ensure)
   (python-mode    . eglot-ensure)
   (python-ts-mode . my/python-shell-setup)
   (python-mode    . my/python-shell-setup)
   (python-ts-mode . (lambda ()
                       (setq-local dash-docs-docsets
                                   '("Python" "SciPy" "Pandas" "Numpy" "xarray"))))
   (python-mode    . (lambda ()
                       (setq-local dash-docs-docsets
                                   '("Python" "SciPy" "Pandas" "Numpy" "xarray")))))
  :config
  ;; rassumfrassum multiplexes ty (type checking) and ruff (diagnostics).
  ;; The 'python' preset is overridden in ~/.config/rassumfrassum/python.py
  ;; to launch both servers via uvx, so neither needs to be in the project.
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("rass" "python")))

  ;; Tell ruff-lsp not to format — apheleia owns formatting via ruff on save.
  ;; The "rass" key routes settings to individual multiplexed servers by name.
  (add-hook 'python-base-mode-hook
            (lambda ()
              (setq-local eglot-workspace-configuration
                          '(:rass (:ruff (:settings (:formatOnSave :json-false)))))))

  ;; Repeat-map for indent-shift (verbatim from config.el §7).
  (defvar-keymap python-indent-shift-right-repeat-map
    :repeat t
    ">" #'python-indent-shift-right
    "<" #'python-indent-shift-left)

  ;; cape-wrap-noninterruptible for python shell completion (verbatim from config.el).
  (advice-add 'python-shell-completion-at-point
              :around #'cape-wrap-noninterruptible))

;; Disable corfu-auto in the Python REPL (replaces setq-hook!).
(add-hook 'inferior-python-mode-hook
          (lambda () (setq-local corfu-auto nil)))

;; Apheleia: ruff + ruff-isort for Python files (verbatim from config.el §7).
(with-eval-after-load 'apheleia
  (setf (alist-get 'ruff apheleia-formatters) (-concat '("uv" "run") (alist-get 'ruff apheleia-formatters)))
  (setf (alist-get 'ruff-isort apheleia-formatters) (-concat '("uv" "run") (alist-get 'ruff-isort apheleia-formatters)))
  (setf (alist-get 'python-mode    apheleia-mode-alist) '(ruff ruff-isort))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff ruff-isort)))

;; Compilation navigation for `ruff check' and `ty check'.  Both default to a
;; Rust-style "full" diagnostic (header line, then " --> file:line:col") and
;; both accept --output-format concise ("file:line:col: ..."), so each tool
;; gets a rule for either shape.  The built-in `rustc' rule doesn't cover
;; them: ruff's header leads with a bare rule code and no severity word, and
;; ty's codes are names (error[invalid-assignment]), not error[E0123].
(with-eval-after-load 'compile
  ;; ruff's text output carries no severity, so everything is an error.
  ;; ty marks each diagnostic, hence the (WARNING . INFO) type groups.
  (dolist (rule
           `((ruff-concise
              ,(concat "^\\(?1:.+?\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\): "
                       "\\(?:[A-Z]+[0-9]+ \\|invalid-syntax: \\)")
              1 2 3)
             (ruff-full
              ,(concat "^\\(?:[A-Z]+[0-9]+ \\|invalid-syntax: \\).*\n"
                       " *--> \\(?1:.+?\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\)$")
              1 2 3)
             (ty-concise
              ,(concat "^\\(?1:.+?\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\): "
                       "\\(?:error\\|\\(?4:warning\\)\\|\\(?5:info\\)\\)"
                       "\\[[a-z0-9-]+\\]")
              1 2 3 (4 . 5))
             (ty-full
              ,(concat "^\\(?:error\\|\\(?4:warning\\)\\|\\(?5:info\\)\\)"
                       "\\[[a-z0-9-]+\\]:.*\n"
                       " *--> \\(?1:.+?\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\)$")
              1 2 3 (4 . 5))))
    (setf (alist-get (car rule) compilation-error-regexp-alist-alist)
          (cdr rule))
    ;; Prepended so these win over the generic `gnu' rule, which would
    ;; otherwise claim the concise lines first.
    (add-to-list 'compilation-error-regexp-alist (car rule))))

;; uv-mode is deliberately not used: it activates a venv by mutating the
;; *global* PATH, VIRTUAL_ENV and `exec-path', leaking one project's venv into
;; every other buffer and fighting envrc.  See `my/python-venv-activate'.

;; uv.el — transient interface to the uv CLI (add/remove/sync/lock/run/...).
;; Complements uv-mode, which only handles venv activation.
(use-package uv
  :ensure (:host github :repo "johannes-mueller/uv.el")
  :commands (uv)
  :init
  (with-eval-after-load 'python
    (keymap-set python-base-mode-map "C-c u" #'uv))
  :config
  ;; uv.el reads pyproject.toml via the TOML tree-sitter grammar.
  (add-to-list 'treesit-language-source-alist
               '(toml "https://github.com/tree-sitter-grammars/tree-sitter-toml"))
  (unless (treesit-language-available-p 'toml)
    (treesit-install-language-grammar 'toml)))

(provide 'lang-python)
;;; lang-python.el ends here
