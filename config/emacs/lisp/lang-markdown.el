;;; lang-markdown.el --- Markdown configuration -*- lexical-binding: t; -*-

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode))
  :custom
  (markdown-command "multimarkdown")
  (markdown-fontify-code-blocks-natively t))

(use-package edit-indirect
  :after (markdown-mode))

(provide 'lang-markdown)
;;; lang-markdown.el ends here
