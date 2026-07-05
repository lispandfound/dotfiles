;;; lang-org.el --- Org-mode configuration -*- lexical-binding: t; -*-

(use-package org
  :ensure nil
  :hook
  ((org-mode . visual-line-mode)
   (org-mode . org-fragtog-mode))
  :custom
  (org-directory "~/org/")
  (org-id-locations-file (expand-file-name "org-id-locations" my/local-dir))
  (org-startup-indented t)
  (org-hide-leading-stars t)
  (org-log-done 'time)
  (org-return-follows-link t)
  :bind (:map my/notes-map
              ("a" . org-agenda)
              ("c" . org-capture)
              ("l" . org-store-link)
              ("t" . org-todo-list)))

;; org-fragtog: auto-toggle LaTeX fragment previews on cursor entry/exit.
;; Moved here from tools/denote where it was logically misplaced.
(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))

;; org-download: drag-and-drop image support (+dragndrop flag).
(use-package org-download
  :after org
  :hook (dired-mode . org-download-enable))

;; ox-slack is loaded in config-tools.el; ensure it loads after ox.
(with-eval-after-load 'ox
  (require 'ox-slack nil t))

(use-package org-re-reveal
  :after org)

(provide 'lang-org)
;;; lang-org.el ends here
