;;; lang/just/config.el -*- lexical-binding: t; -*-


(use-package! just-ts-mode
  :config
    (set-formatter! 'just-format
        '("just" "--fmt" "--unstable" "--justfile" inplace)
        :modes '(just-ts-mode)))
