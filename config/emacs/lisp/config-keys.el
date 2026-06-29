;;; config-keys.el --- Keybindings and keyboard translations -*- lexical-binding: t; -*-

;;; =========================================================================
;;; KEYBOARD TRANSLATIONS
;;; C-t and C-x are swapped. C-t acts as the C-x prefix; C-x transposes chars.
;;; =========================================================================

(keyboard-translate ?\C-t ?\C-x)
(keyboard-translate ?\C-x ?\C-t)

;; Re-apply translations for each new daemon frame.
(add-hook 'server-after-make-frame-hook
          (lambda ()
            (keyboard-translate ?\C-t ?\C-x)
            (keyboard-translate ?\C-x ?\C-t)))

(global-unset-key (kbd "C-t"))

;;; =========================================================================
;;; GLOBAL MODES
;;; =========================================================================

(repeat-mode 1)
(setq view-read-only t)

;;; =========================================================================
;;; DOOM HOOK SHIMS
;;; Simulate doom-first-input-hook and doom-first-file-hook for packages
;;; that reference them (none in this config, but useful as a pattern).
;;; =========================================================================

(defvar my/first-input-hook nil
  "Hooks run once on the first user input, then removed.")

(defun my/run-first-input-hook ()
  (run-hooks 'my/first-input-hook)
  (remove-hook 'pre-command-hook #'my/run-first-input-hook))

(add-hook 'pre-command-hook #'my/run-first-input-hook)

(defvar my/first-file-hook nil
  "Hooks run once when the first real file is visited, then removed.")

(defun my/run-first-file-hook ()
  (when buffer-file-name
    (run-hooks 'my/first-file-hook)
    (remove-hook 'find-file-hook #'my/run-first-file-hook)))

(add-hook 'find-file-hook #'my/run-first-file-hook)

;;; =========================================================================
;;; GLOBAL KEYBINDINGS
;;; =========================================================================

(keymap-global-set "M-/" #'hippie-expand)
(keymap-global-set "M-r" #'query-replace-regexp)
(keymap-global-set "M-u" #'upcase-dwim)
(keymap-global-set "M-l" #'downcase-dwim)
(keymap-global-set "M-c" #'capitalize-dwim)
(keymap-global-set "C-'" #'imenu)            ; Doom default: fast imenu jump
(keymap-global-set "C-S-r" #'vertico-repeat) ; re-open last vertico session

;; expand-region — bound here so it works once the package loads.
(keymap-global-set "C-=" #'er/expand-region)

;; Jump to the MRU window — fast two-window back-and-forth (see config-windows.el).
(keymap-global-set "M-o" #'my/other-window-mru)
;; ace-window extras: one-command dispatch and next-buffer routing.
;; Physical keys: C-t S-o and C-t 4 o (C-x is swapped with C-t).
(keymap-global-set "C-x O"   #'my/ace-window-one-command)
(keymap-global-set "C-x 4 o" #'my/ace-window-prefix)

;;; =========================================================================
;;; LEADER KEYMAPS (C-c prefix replaces Doom's SPC leader)
;;; =========================================================================

(defvar-keymap my/buffer-map
  :doc "Buffer commands (C-c b)"
  "b" #'consult-buffer
  "B" #'consult-buffer-other-window
  "d" #'kill-current-buffer
  "i" #'ibuffer
  "n" #'next-buffer
  "p" #'previous-buffer
  "]" #'next-buffer
  "[" #'previous-buffer
  "r" #'revert-buffer
  "R" #'rename-buffer
  "s" #'save-buffer
  "m" #'bookmark-set
  "M" #'bookmark-delete
  "z" #'bury-buffer
  "x" #'scratch-buffer)

(defvar-keymap my/code-map
  :doc "Code commands (C-c c)"
  "a" #'eglot-code-actions
  "c" #'compile
  "C" #'recompile
  "d" #'xref-find-definitions
  "D" #'xref-find-references
  "f" #'eglot-format-buffer
  "i" #'eglot-find-implementation
  "k" #'eldoc
  "r" #'eglot-rename
  "w" #'delete-trailing-whitespace
  "x" #'flymake-show-buffer-diagnostics
  "X" #'flymake-show-project-diagnostics)

(defvar-keymap my/dape-map
  :doc "Dape debugger commands (C-c D)"
  "d" #'dape
  "c" #'dape-continue
  "n" #'dape-next
  "s" #'dape-step-in
  "o" #'dape-step-out
  "p" #'dape-pause
  "r" #'dape-restart
  "R" #'dape-repl
  "i" #'dape-info
  "b" #'dape-breakpoint-toggle
  "B" #'dape-breakpoint-remove-all
  "e" #'dape-evaluate-expression
  "w" #'dape-watch-dwim
  "q" #'dape-quit
  "D" #'dape-disconnect-quit)

(defvar-keymap my/lookup-map
  :doc "Lookup commands (C-c l)")

(defvar-keymap my/notes-map
  :doc "Notes and gist commands (C-c n)")

(defvar-keymap my/project-map
  :doc "Project commands (C-c p)"
  "f" #'project-find-file
  "p" #'project-switch-project
  "b" #'consult-project-buffer
  "d" #'project-dired
  "g" #'consult-ripgrep
  "k" #'project-kill-buffers
  "e" #'project-eshell
  "c" #'project-compile
  "o" #'find-sibling-file
  "&" #'project-async-shell-command
  "!" #'project-shell-command)

(defvar-keymap my/vc-map
  :doc "Version control commands (C-c v)")

(defvar-keymap my/denote-map
  :doc "Denote note commands (C-c d)")

(defvar-keymap my/denote-bk-map
  :doc "Denote bookkeeping (C-c d b)")

(defvar-keymap my/denote-link-map
  :doc "Denote linking (C-c d l)")

(defvar-keymap my/denote-search-map
  :doc "Denote searching (C-c d s)")

(defvar-keymap my/toggle-map
  :doc "Toggle minor modes (C-c t)"
  "c" #'global-display-fill-column-indicator-mode
  "d" #'diff-hl-mode
  "f" #'flymake-mode
  "F" #'toggle-frame-fullscreen
  "l" #'display-line-numbers-mode
  "p" #'prettify-symbols-mode
  "r" #'read-only-mode
  "s" #'flyspell-mode
  "v" #'visible-mode
  "w" #'visual-line-mode)

(defvar-keymap my/window-map
  :doc "Window / workspace commands (C-c w)"
  ;; Splits (mnemonic: - splits below, | splits right)
  "-" #'split-window-below
  "|" #'split-window-right
  ;; Delete windows
  "d" #'delete-window
  "o" #'delete-other-windows
  ;; winner-mode undo/redo
  "u" #'winner-undo
  "U" #'winner-redo
  ;; Popup terminal
  "t" #'my/popup-eshell
  ;; tab-bar workspace commands
  "N" #'tab-new
  "c" #'tab-close
  "r" #'tab-rename
  "n" #'tab-next
  "p" #'tab-previous
  "w" #'tab-switch
  "1" (lambda () (interactive) (tab-bar-select-tab 1))
  "2" (lambda () (interactive) (tab-bar-select-tab 2))
  "3" (lambda () (interactive) (tab-bar-select-tab 3))
  "4" (lambda () (interactive) (tab-bar-select-tab 4))
  "5" (lambda () (interactive) (tab-bar-select-tab 5))
  "6" (lambda () (interactive) (tab-bar-select-tab 6))
  "7" (lambda () (interactive) (tab-bar-select-tab 7))
  "8" (lambda () (interactive) (tab-bar-select-tab 8))
  "9" (lambda () (interactive) (tab-bar-select-tab 9)))

(defvar-keymap my/file-map
  :doc "File commands (C-c f)"
  "f" #'find-file
  "d" #'dired
  "e" #'my/find-emacs-config
  "r" #'recentf-open
  "R" #'revert-buffer
  "s" #'save-buffer
  "S" #'write-file
  "D" #'my/delete-this-file
  "m" #'my/move-this-file
  "y" #'my/yank-buffer-path
  "Y" #'my/yank-buffer-path-relative)

(defvar-keymap my/open-map
  :doc "Open things (C-c o)"
  "b" #'browse-url
  "d" #'dired-jump
  "D" #'docker
  "e" #'eshell
  "t" #'my/popup-eshell
  "f" #'make-frame)

;; Register prefix maps under C-c
(keymap-global-set "C-c b" my/buffer-map)
(keymap-global-set "C-c c" my/code-map)
(keymap-global-set "C-c d" my/denote-map)
(keymap-global-set "C-c D" my/dape-map)
(keymap-global-set "C-c f" my/file-map)
(keymap-global-set "C-c l" my/lookup-map)
(keymap-global-set "C-c n" my/notes-map)
(keymap-global-set "C-c o" my/open-map)
(keymap-global-set "C-c p" my/project-map)
(keymap-global-set "C-c t" my/toggle-map)
(keymap-global-set "C-c v" my/vc-map)
(keymap-global-set "C-c w" my/window-map)
(keymap-set my/denote-map "b" my/denote-bk-map)
(keymap-set my/denote-map "l" my/denote-link-map)
(keymap-set my/denote-map "s" my/denote-search-map)

;; File operation helpers used by my/file-map.
(defun my/delete-this-file ()
  "Delete the current file and kill its buffer."
  (interactive)
  (unless buffer-file-name (user-error "Buffer has no file"))
  (when (yes-or-no-p (format "Delete %s? " buffer-file-name))
    (delete-file buffer-file-name t)
    (kill-current-buffer)))

(defun my/move-this-file (new-name)
  "Rename/move the current file to NEW-NAME."
  (interactive (list (read-file-name "Move to: " (file-name-directory buffer-file-name))))
  (unless buffer-file-name (user-error "Buffer has no file"))
  (rename-file buffer-file-name new-name)
  (set-visited-file-name new-name t t))

(defun my/find-emacs-config ()
  "Open the Emacs config directory in dired."
  (interactive)
  (dired user-emacs-directory))

(defun my/yank-buffer-path ()
  "Copy the absolute path of the current buffer's file."
  (interactive)
  (if buffer-file-name
      (progn (kill-new buffer-file-name)
             (message "Copied: %s" buffer-file-name))
    (user-error "Buffer has no file")))

(defun my/yank-buffer-path-relative ()
  "Copy path of current buffer's file relative to the project root."
  (interactive)
  (if buffer-file-name
      (let* ((proj (project-current))
             (root (if proj (project-root proj) default-directory))
             (rel  (file-relative-name buffer-file-name root)))
        (kill-new rel)
        (message "Copied: %s" rel))
    (user-error "Buffer has no file")))

;; upcase-variable macro (SPC c u → C-c c u)
(defalias 'upcase-variable (kmacro "C-= = C-x C-u"))
(keymap-set my/code-map "u" #'upcase-variable)

(provide 'config-keys)
;;; config-keys.el ends here
