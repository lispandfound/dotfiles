;;; lang-just.el --- Justfile configuration -*- lexical-binding: t; -*-

;; Replaces lang/just custom Doom module.
(use-package just-ts-mode
  :config
  ;; Replaces: (set-formatter! 'just-format '("just" "--fmt" "--unstable" "--justfile" inplace) :modes '(just-ts-mode))
  (with-eval-after-load 'apheleia
    (setf (alist-get 'just-fmt apheleia-formatters)
          '("just" "--fmt" "--unstable" "--justfile" filepath))
    (setf (alist-get 'just-ts-mode apheleia-mode-alist) '(just-fmt))))

(provide 'lang-just)
;;; lang-just.el ends here
