
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-selection
   '(((output-dvi has-no-display-manager) "dvi2tty")
     ((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi")
     (output-pdf "Okular") (output-pdf "xdg-open")))
 '(casual-lib-use-unicode t)
 '(consult-narrow-key "<")
 '(dired-compress-file-default-suffix ".xz")
 '(display-time-default-load-average nil)
 '(display-time-format "%H:%M")
 '(mood-line-glyph-alist
   '((:checker-info . 8505) (:checker-issues . 9873)
     (:checker-good . 10004) (:checker-checking . 58726)
     (:checker-errored . 10006) (:checker-interrupted . 9208)
     (:vc-added . 43) (:vc-needs-merge . 10231)
     (:vc-needs-update . 8595) (:vc-conflict . 10006)
     (:vc-good . 10004) (:buffer-narrowed . 9660)
     (:buffer-modified . 9679) (:buffer-read-only . 9632)
     (:frame-client . 8645) (:count-separator . 10005)))
 '(org-agenda-files nil nil nil "Customized with use-package org")
 '(org-file-apps
   '(("\\.pdf\\'" . system) (auto-mode . emacs) (directory . emacs)
     ("\\.mm\\'" . default) ("\\.x?html?\\'" . default)))
 '(package-install-upgrade-built-in t)
 '(package-selected-packages
   '(all-the-icons-ibuffer apheleia apptainer-mode ascii-art-to-unicode
                           auctex avy better-defaults cape casual
                           cmake-mode comint-mime comment-dwim-2
                           consult-dir consult-notes copilot-chat
                           corfu crux cylc-mode cython-mode deadgrep
                           denote-journal denote-org detached devdocs
                           dumb-jump edit-indirect edit-server
                           elisp-demos elm-mode embark-consult embrace
                           envrc exec-path-from-shell expreg
                           flymake-collection forge gist git-link
                           gitignore-templates grip-mode haskell-mode
                           helpful jenkinsfile-mode jinx just-mode
                           ligature magit-lfs marginalia mermaid-mode
                           modus-themes mood-line nix-ts-mode
                           orderless org-download org-fragtog org-menu
                           org-present ox-reveal ox-slack
                           pandoc-transient popper pr-review pyvenv
                           restart-emacs rustic tempel-collection
                           titlecase tldr transpose-frame treesit-auto
                           typst-ts-mode uniline vertico
                           visual-fill-column wgrep ws-butler
                           yaml-mode))
 '(package-vc-selected-packages
   '((cylc-mode :url "https://github.com/cylc/cylc-flow" :lisp-dir
                "cylc/flow/etc/syntax/")
     (typst-ts-mode :url
                    "https://codeberg.org/meow_king/typst-ts-mode")
     (pandoc-transient :url
                       "https://github.com/lispandfound/pandoc-transient")
     (org-menu :url "https://github.com/sheijk/org-menu")
     (apptainer-mode :url "https://github.com/jrgant/apptainer-mode")
     (copilot-chat :url "https://github.com/chep/copilot-chat.el"
                   :branch "shell-maker-update")
     (copilot :url "https://github.com/copilot-emacs/copilot.el"
              :branch "main")
     (skempo :url "https://github.com/xFA25E/skempo")))
 '(popper-reference-buffers
   '("\\*tldr\\*" "Output\\*$" "\\*Async Shell Command\\*"
     compilation-mode help-mode eshell-mode "\\*Messages\\*$"
     devdocs-mode inferior-python-mode occur-mode "\\*Embark"
     detached-compilation-mode helpful-mode))
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
 '(safe-local-variable-values
   '((projectile-project-test-cmd . "uv run pytest -s tests/")))
 '(shackle-mode t)
 '(shackle-rules '((help-mode :popup t) (eshell-mode :popup t)))
 '(use-package-always-defer t)
 '(use-package-always-ensure t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#ffffff" :foreground "#000000" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 110 :width normal :foundry "JB" :family "JetBrainsMono Nerd Font"))))
 '(mode-line-active ((((class color) (min-colors 256)) :box (:line-width 5 :color "#c8c8c8"))))
 '(mode-line-inactive ((((class color) (min-colors 256)) :box (:line-width 5 :color "#e6e6e6")))))
