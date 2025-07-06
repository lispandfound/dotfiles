
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(casual-lib-use-unicode t)
 '(consult-narrow-key "<")
 '(dired-compress-file-default-suffix ".xz")
 '(display-line-numbers t)
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
 '(org-capture-templates
   '(("l" "Logged completed task" entry
      (file+headline org-agenda-capture-file "Tasks")
      "* DONE %?\12 %U\12 %a\12 %i")
     ("n" "Note" entry (file+headline org-default-notes-file "Notes")
      "* %?\12 %U\12 %a\12 %i")
     ("t" "Todo" entry (file+headline org-agenda-capture-file "Tasks")
      "* TODO %?\12 %U\12 %a\12 %i")))
 '(package-install-upgrade-built-in t)
 '(package-selected-packages
   '(apptainer-mode copilot copilot-chat org-menu pandoc-transient
                    projectile skempo uniline))
 '(package-vc-selected-packages
   '((copilot-chat :url "https://github.com/chep/copilot-chat.el" :branch
                   "shell-maker-update")
     (copilot :url "https://github.com/copilot-emacs/copilot.el"
              :branch "main")
     (apptainer-mode :vc-backend Git :url
                     "https://github.com/jrgant/apptainer-mode")
     (org-menu :url "https://github.com/sheijk/org-menu")
     (pandoc-transient :url
                       "https://github.com/lispandfound/pandoc-transient")
     (skempo :url "https://github.com/xFA25E/skempo")))
 '(popper-reference-buffers
   '("\\*tldr\\*" "Output\\*$" "\\*Async Shell Command\\*"
     compilation-mode help-mode eshell-mode "\\*Messages\\*$"
     devdocs-mode "\\*Detached Shell Command\\*"))
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
 '(mode-line ((t (:box (:line-width (2 . 6) :style flat-button)))))
 '(mode-line-inactive ((t (:box (:line-width (2 . 6) :style flat-button))))))
