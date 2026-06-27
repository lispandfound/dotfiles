;;; lang-sh.el --- Shell script configuration -*- lexical-binding: t; -*-

;; bash-ts-mode is built-in (Emacs 29+); treesit-auto handles remapping.

(use-package sh-script
  :ensure nil
  :hook
  ((sh-mode      . my/eglot-ensure-unless-remote)
   (bash-ts-mode . my/eglot-ensure-unless-remote))
  :config
  (add-to-list 'eglot-server-programs
               '((sh-mode bash-ts-mode) . ("bash-language-server" "start"))))

(provide 'lang-sh)
;;; lang-sh.el ends here
