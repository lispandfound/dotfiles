;;; lang-cc.el --- C/C++ and CMake configuration -*- lexical-binding: t; -*-

;; c-ts-mode and c++-ts-mode are built-in (Emacs 29+).
;; treesit-auto in config-tools.el handles remapping from c-mode/c++-mode.

(use-package cc-mode
  :ensure nil
  :hook
  ((c-ts-mode   . eglot-ensure)
   (c++-ts-mode . eglot-ensure)
   (c-mode      . eglot-ensure)
   (c++-mode    . eglot-ensure)))

(with-eval-after-load 'cmake-mode
  (add-hook 'cmake-mode-hook #'eglot-ensure))

(provide 'lang-cc)
;;; lang-cc.el ends here
