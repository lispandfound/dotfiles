
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compile-command "just ")
 '(consult-narrow-key "<")
 '(display-line-numbers t)
 '(org-capture-templates
   '(("n" "Note" entry (file+headline "~/org/todo.org" "Notes")
      "* %?\12 %U\12 %a\12 %i")
     ("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
      "* TODO %?\12 %U\12 %a\12 %i")))
 '(package-install-upgrade-built-in t)
 '(package-selected-packages
   '(adaptive-wrap apheleia ascii-art-to-unicode auto-virtualenv
                   better-defaults browse-at-remote cape casual-avy
                   casual-calc casual-dired casual-info casual-isearch
                   casual-suite consult-dir consult-eglot corfu crux
                   csv-mode devdocs doom-themes dumb-jump
                   edit-indirect elm-mode embark-consult embrace
                   exec-path-from-shell expreg forge gptel grip-mode
                   haskell-mode htmlize ibuffer-project jinx just-mode
                   ligature marginalia mermaid-mode moody numpydoc
                   orderless org-menu org-reveal ox-reveal
                   pandoc-transient skempo titlecase transpose-frame
                   treesit-auto vertico visual-fill-column wgrep
                   ws-butler yaml-mode zoxide))
 '(package-vc-selected-packages
   '((org-menu :url "https://github.com/sheijk/org-menu")
     (pandoc-transient :url
                       "https://github.com/lispandfound/pandoc-transient")
     (skempo :url "https://github.com/xFA25E/skempo")))
 '(register-preview-delay 0.5)
 '(use-package-always-defer t)
 '(use-package-always-ensure t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#282c34" :foreground "#bbc2cf" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 98 :width normal :foundry "JB" :family "JetBrains Mono")))))
