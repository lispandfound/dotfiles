;;; lang/typst/config.el -*- lexical-binding: t; -*-

(use-package! websocket)
(use-package! typst-preview
  :config
  (setq typst-preview-browser "default")
  (define-key typst-preview-mode-map (kbd "C-c C-j") 'typst-preview-send-position)
  )

(use-package! typst-ts-mode
  :custom
  (typst-ts-mode-watch-options "--open")
  :config
  ;; make sure to install typst-lsp from
  ;; https://github.com/nvarner/typst-lsp/releases
  ;; or use tinymist
  (after! lsp
	(add-to-list 'lsp-language-id-configuration '(typst-ts-mode . "typst"))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection "typst-lsp")
    :major-modes '(typst-ts-mode)
    :server-id 'typst-lsp))))
