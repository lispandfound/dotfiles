;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'modus-operandi)
(setq doom-font (font-spec
                 :inherit nil
                 :extend nil
                 :stipple nil
                 :inverse-video nil
                 :box nil
                 :strike-through nil
                 :overline nil
                 :underline nil
                 :slant 'normal
                 :weight 'regular
                 :size 14.0
                 :width 'normal
                 :foundry "JB"
                 :family "JetBrainsMono Nerd Font"))
(repeat-mode)
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

(keyboard-translate ?\C-t ?\C-x)
(keyboard-translate ?\C-x ?\C-t)
(global-unset-key (kbd "C-t"))          ; unbind the transpose-char key because it annoys me
(map! "C-t C-s" #'save-buffer) ; Occasionally C-x is C-t
(setq tramp-use-connection-share nil)

(use-package! slurm-script-mode)
(use-package! slurm-mode)

(map!
 "C-." #'embark-act
 "M-/" #'hippie-expand
 "M-r" #'query-replace-regexp)

(use-package! igist
  :init
  (map!
   :leader
   "ng" #'igist-dispatch))

(after! python
  (setq lsp-pyright-langserver-command "basedpyright")
  (defvar-keymap python-indent-shift-right-repeat-map
    :repeat t
    ">" #'python-indent-shift-right
    "<" #'python-indent-shift-left))

(after! (:and python apheleia)
  (push '(python-mode . ruff) apheleia-mode-alist)
  (push '(python-ts-mode . ruff) apheleia-mode-alist)


  )

(setenv "DICTIONARY" "en_AU")
(setq ispell-local-dictionary "en_AU")
(setq ispell-local-dictionary-alist
      '(("en_AU" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_AU") nil utf-8)))

(setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s))


(after! latex
  (defun conf/enable-shell-escape ()
    (setq-local TeX-command-extra-options "--shell-escape"))
  (add-hook! latex-mode-hook #'conf/enable-shell-escape))

(use-package! pet
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10)
  (add-hook 'python-mode-hook
            (lambda ()
              (setq-local python-shell-interpreter (pet-executable-find "python")
                          python-shell-virtualenv-root (pet-virtualenv-root))))
  (add-hook 'python-mode-hook 'pet-flycheck-setup))

(use-package! cylc-mode
  :mode ("suite.*\\.rc\\'" "\\.cylc\\'"))

(use-package! ox-slack :after (org))
(use-package! cmake-mode)
(use-package! magit-lfs
  :after (magit))
(use-package! dwim-shell-command
  :ensure t
  :bind (([remap shell-command] . dwim-shell-command)
         :map dired-mode-map
         ([remap dired-do-async-shell-command] . dwim-shell-command)
         ([remap dired-do-shell-command] . dwim-shell-command)
         ([remap dired-smart-shell-command] . dwim-shell-command)))

(use-package! dwim-shell-commands :after dwim-shell-command)

(use-package! detached
  :init
  (detached-init)
  :bind (;; Replace `async-shell-command' with `detached-shell-command'
         ([remap async-shell-command] . detached-shell-command)
         ;; Replace `compile' with `detached-compile'
         ([remap compile] . detached-compile)
         ([remap recompile] . detached-compile-recompile)
         ;; Replace built in completion of sessions with `consult'
         ([remap detached-open-session] . detached-consult-session))
  :custom ((detached-show-output-on-attach t)
           (detached-terminal-data-command system-type)))

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
