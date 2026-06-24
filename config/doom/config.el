;; =============================================================================
;; 1. CORE UI & APPERANCE SETTINGS
;; =============================================================================

;; Theme configuration
(setq doom-theme 'modus-operandi)

;; Comprehensive font settings for JetBrainsMono
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

;; Line numbers styling (t turns them on globally)
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; =============================================================================
;; 2. ENVIRONMENT, LOCALE & SYSTEM
;; =============================================================================

;; Base directories
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Dictionary and spellcheck customization (en_AU)
(setenv "DICTIONARY" "en_AU")
(setq ispell-local-dictionary "en_AU")
(setq ispell-local-dictionary-alist
      '(("en_AU" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_AU") nil utf-8)))

;; Lookup provider configurations
(when (modulep! :tools lookup)
  (add-to-list '+lookup-provider-url-alist '("Hoogle" "https://hoogle.mangoiv.com/?q=%s")))


;; =============================================================================
;; 3. KEYBINDINGS, RE-MAPPINGS & GLOBAL MODES
;; =============================================================================

;; Enable global repeat-mode for sequence chaining
(repeat-mode)

;; Global key re-mappings and translations (Swap C-x and C-t)
(keyboard-translate ?\C-t ?\C-x)
(keyboard-translate ?\C-x ?\C-t)

(add-hook 'server-after-make-frame-hook
          (lambda ()
            (keyboard-translate ?\C-t ?\C-x)
            (keyboard-translate ?\C-x ?\C-t)))

(global-unset-key (kbd "C-t"))          ; unbind the transpose-char key because it annoys me

;; Global general map shortcuts
(map!
 "C-." #'embark-act
 "M-/" #'hippie-expand
 "M-r" #'query-replace-regexp)


;; =============================================================================
;; 4. TRAMP & REMOTE FILE OPTIMIZATIONS
;; =============================================================================

;; Direct performance flags for remote connections
(setq remote-file-name-inhibit-locks t
      tramp-use-scp-direct-remote-copying t
      remote-file-name-inhibit-auto-save-visited t)

(setq tramp-copy-size-limit (* 1024 1024) ;; 1MB
      tramp-verbose 2)

;; Let system ~/.ssh/config handle connection multiplexing sharing options
(setq tramp-use-connection-share nil)

;; Set up profile for direct async remote processes
(connection-local-set-profile-variables
 'remote-direct-async-process
 '((tramp-direct-async-process . t)))

;; Apply direct async process properties exclusively to SCP transfers
(connection-local-set-profiles
 '(:application tramp :protocol "scp")
 'remote-direct-async-process)

(connection-local-set-profiles
 '(:application tramp :protocol "ssh")
 'remote-direct-async-process)

(connection-local-set-profiles
 '(:application tramp :protocol "rsync")
 'remote-direct-async-process)

;; TRAMP speedup UI hooks
(defun my-tramp-optimization-hook ()
  "Strip away heavy Doom background features when working over TRAMP."
  (when (file-remote-p default-directory)
    ;; 1. Disable version control completely for this remote buffer (massive Dired boost)
    (setq-local vc-handled-backends nil)
    
    ;; 2. Disable line numbers (a well-known TRAMP file-visiting lag culprit)
    (display-line-numbers-mode -1)
    
    ;; 3. Turn off git-gutter tracking on remote files
    (when (fboundp 'git-gutter-mode) 
      (git-gutter-mode -1))
    
    ;; 4. Simplify the modeline path rendering to stop it from probing parent paths
    (setq-local doom-modeline-buffer-file-name-style 'file-name)))

(add-hook 'find-file-hook #'my-tramp-optimization-hook)
(add-hook 'dired-mode-hook #'my-tramp-optimization-hook)

;; Shared remote memoization logic helper
(defun memoize-remote (key cache orig-fn &rest args)
  "Memoize a value if the key is a remote path."
  (if (and key
           (file-remote-p key))
      (if-let ((current (assoc key (symbol-value cache))))
          (cdr current)
        (let ((current (apply orig-fn args)))
          (set cache (cons (cons key current) (symbol-value cache)))
          current))
    (apply orig-fn args)))


;; =============================================================================
;; 5. GENERAL & EXTENSION PACKAGE CONFIGURATIONS
;; =============================================================================

;; --- Avy ---
(setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s))

(defun avy-zap-up-to-char ()
  "Prompt for a char, zap up to (but not including) the selected instance."
  (interactive)
  (let ((start (point)))
    (avy-with avy-zap-up-to-char
              (call-interactively #'avy-goto-char)
              (kill-region start (point)))))

(define-key! [remap zap-to-char] #'avy-zap-up-to-char)

(setq avy-all-windows t)

;; --- Detached ---
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
           (detached-notification-function (lambda (_) nil))
           (detached-terminal-data-command system-type)))

;; --- DWIM Shell Command ---
(use-package! dwim-shell-command
  :ensure t
  :bind (([remap shell-command] . dwim-shell-command)
         :map dired-mode-map
         ([remap dired-do-async-shell-command] . dwim-shell-command)
         ([remap dired-do-shell-command] . dwim-shell-command)
         ([remap dired-smart-shell-command] . dwim-shell-command)))

(use-package! dwim-shell-commands :after dwim-shell-command)

;; --- Edit Server ---
(use-package! edit-server
  :ensure t
  :commands edit-server-start
  :init (if after-init-time
            (edit-server-start)
          (add-hook 'after-init-hook
                    #'(lambda() (edit-server-start))))
  :config (setq edit-server-new-frame-alist
                '((name . "Edit with Emacs FRAME")
                  (top . 200)
                  (left . 200)
                  (width . 80)
                  (height . 25)
                  (minibuffer . t)
                  (menu-bar-lines . t))))

;; --- GPTel ---
(after! gptel
  (gptel-make-gh-copilot "Copilot"))

;; --- iGist ---
(use-package! igist
  :init
  (map!
   :leader
   "ng" #'igist-dispatch))


;; =============================================================================
;; 6. VERSION CONTROL & MAGIT CONFIGURATIONS
;; =============================================================================

;; --- Git Link ---
(use-package! git-link)
(map! :leader "v l y" 'git-link)

;; --- Magit ---
(setq magit-tramp-pipe-stty-settings 'pty)

(after! magit
  ;; Memoize magit top level
  (defvar magit-toplevel-cache nil)
  (defun memoize-magit-toplevel (orig &optional directory)
    (memoize-remote (or directory default-directory)
                    'magit-toplevel-cache orig directory))
  (advice-add 'magit-toplevel :around #'memoize-magit-toplevel)
  ;; Set magit to use auth source for passwords
  (setq magit-process-find-password-functions
        '(magit-process-password-auth-source)))


;; =============================================================================
;; 7. LANGUAGE SPECIFIC CONFIGURATIONS & MACROS
;; =============================================================================

;; --- Custom Functional Macros ---
(defalias 'upcase-variable
  (kmacro "C-= = C-x C-u"))
(map! :leader "c u" 'upcase-variable)

;; --- Cylc Mode ---
(use-package! cylc-mode
  :mode ("suite.*\\.rc\\'" "\\.cylc\\'"))

;; --- LaTeX Mode ---
(after! latex
  (defun conf/enable-shell-escape ()
    (setq-local TeX-command-extra-options "--shell-escape"))
  (add-hook! latex-mode-hook #'conf/enable-shell-escape))

;; --- LSP Configuration ---
(after! lsp
  (add-to-list 'lsp-file-watch-ignored-directories "\\.zarr\\'"))

;; --- Python Mode ---
(set-docsets! 'python-base-mode "Python" "SciPy" "Pandas" "Numpy" "xarray")

(after! python
  (defvar-keymap python-indent-shift-right-repeat-map
    :repeat t
    ">" #'python-indent-shift-right
    "<" #'python-indent-shift-left))




(after! apheleia
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff-isort ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff)))

(after! python
  (advice-add 'python-shell-completion-at-point :around #'cape-wrap-noninterruptible))

(setq-hook! 'inferior-python-mode-hook corfu-auto nil)


(after! xref
  (remove-hook 'xref-backend-functions #'etags--xref-backend))

(setq view-read-only t)

(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-c") 'capitalize-dwim)

(after! gptel
  (setq-default gptel-backend (gptel-make-ollama "Ollama-Cloud"
                                :host "localhost:11434"
                                :stream t
                                :models '("deepseek-v4-flash:cloud"))
                gptel-model "deepseek-v4-flash:cloud"))
