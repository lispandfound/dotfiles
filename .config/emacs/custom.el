
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compile-command "just ")
 '(consult-narrow-key "<")
 '(display-line-numbers t)
 '(display-time-default-load-average nil)
 '(display-time-format "%H:%M")
 '(org-capture-templates
   '(("l" "Logged completed task" entry
      (file+headline "~/org/todo.org" "Tasks")
      "* DONE %?\12 %U\12 %a\12 %i")
     ("n" "Note" entry (file+headline "~/org/todo.org" "Notes")
      "* %?\12 %U\12 %a\12 %i")
     ("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
      "* TODO %?\12 %U\12 %a\12 %i")))
 '(package-install-upgrade-built-in t)
 '(package-selected-packages
   '(adaptive-wrap apheleia apptainer-mode ascii-art-to-unicode
                   auto-virtualenv better-defaults browse-at-remote
                   cape casual-suite comment-dwim-2 consult-dir
                   consult-eglot corfu counsel-pydoc crux csv-mode
                   devdocs doom-themes dumb-jump edit-indirect
                   elm-mode embark-consult embrace
                   exec-path-from-shell expreg forge gist gptel
                   grip-mode haskell-mode htmlize ibuffer-project jinx
                   just-mode ligature marginalia meow mermaid-mode
                   mood-line moody numpydoc orderless org-menu
                   ox-reveal pandoc-transient popper pydoc shackle
                   skempo titlecase tldr transpose-frame treesit-auto
                   try vertico visual-fill-column wgrep ws-butler
                   yaml-mode zoxide))
 '(package-vc-selected-packages
   '((apptainer-mode :vc-backend Git :url
                     "https://github.com/jrgant/apptainer-mode")
     (org-menu :url "https://github.com/sheijk/org-menu")
     (pandoc-transient :url
                       "https://github.com/lispandfound/pandoc-transient")
     (skempo :url "https://github.com/xFA25E/skempo")))
 '(popper-reference-buffers
   '("\\*tldr\\*" "Output\\*$" "\\*Async Shell Command\\*"
     compilation-mode help-mode eshell-mode "\\*Messages\\*$"
     devdocs-mode))
 '(popper-window-height 20)
 '(proced-auto-update-flag 'visible)
 '(proced-auto-update-interval 1)
 '(proced-enable-color-flag t)
 '(proced-filter 'all)
 '(proced-format-alist
   '((short user pid tree pcpu pmem start time (args comm))
     (medium user pid tree pcpu pmem vsize rss ttname state start time
             (args comm))
     (long user euid group pid tree pri nice pcpu pmem vsize rss
           ttname state start time (args comm))
     (verbose user euid group egid pid ppid tree pgrp sess pri nice
              pcpu pmem state thcount vsize rss ttname tpgid minflt
              majflt cminflt cmajflt start time utime stime ctime
              cutime cstime etime (args comm))))
 '(proced-goal-attribute nil)
 '(proced-show-remote-processes t)
 '(proced-tree-flag t)
 '(register-preview-delay 0.5)
 '(shackle-mode t)
 '(shackle-rules '((help-mode :popup t) (eshell-mode :popup t)))
 '(use-package-always-defer t)
 '(use-package-always-ensure t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#282c34" :foreground "#bbc2cf" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 98 :width normal :foundry "JB" :family "JetBrains Mono"))))
 '(mode-line ((t (:background "#1d2026" :foreground "#bbc2cf" :box (:line-width (2 . 6) :style flat-button)))))
 '(mode-line-inactive ((t (:background "#21242b" :foreground "#5B6268" :box (:line-width (2 . 6) :style flat-button))))))
