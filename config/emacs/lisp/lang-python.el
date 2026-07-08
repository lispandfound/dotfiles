;;; lang-python.el --- Python configuration -*- lexical-binding: t; -*-

;; python-ts-mode is built-in (Emacs 29+); treesit-auto handles remapping.

(defcustom my/python-scratchpad-dir "~/src/scratchpad/"
  "uv project to run the Python REPL in when the current buffer has no
project-local uv environment (see `my/python-uv-project-root')."
  :type 'directory
  :group 'python)

(defun my/python-uv-project-root ()
  "Return the uv project directory to use for this buffer's REPL.
Uses the buffer's own uv project when `uv-mode-root' finds a `.venv',
otherwise falls back to `my/python-scratchpad-dir'."
  (directory-file-name
   (expand-file-name (or (uv-mode-root) my/python-scratchpad-dir))))

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
  ((python-ts-mode . eglot-ensure)
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
  (setf (alist-get 'python-mode    apheleia-mode-alist) '(ruff-isort ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff)))

(use-package uv-mode
  ;; :demand so `uv-mode-root' is defined before `my/python-shell-setup'
  ;; (hooked onto the same modes, above) ever needs to call it.
  :demand t
  :hook ((python-mode python-ts-mode) . uv-mode-auto-activate-hook))

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
