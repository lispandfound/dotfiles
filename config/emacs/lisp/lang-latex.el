;;; lang-latex.el --- LaTeX / AUCTeX configuration -*- lexical-binding: t; -*-

(use-package auctex
  :hook
  ((latex-mode . eglot-ensure)
   (latex-mode . cdlatex-mode)
   (latex-mode . conf/enable-shell-escape)
   (LaTeX-mode . eglot-ensure)
   (LaTeX-mode . cdlatex-mode)
   (LaTeX-mode . conf/enable-shell-escape))
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-PDF-mode t)
  (TeX-source-correlate-mode t)
  (TeX-source-correlate-method 'synctex)
  :config
  (defun conf/enable-shell-escape ()
    "Enable --shell-escape for LaTeX compilation."
    (setq-local TeX-command-extra-options "--shell-escape"))

  (add-to-list 'eglot-server-programs
               '((latex-mode LaTeX-mode) . ("texlab")))

  ;; Use pdf-tools as the PDF viewer.
  (with-eval-after-load 'pdf-tools
    (add-to-list 'TeX-view-program-selection
                 '(output-pdf "PDF Tools"))
    (add-to-list 'TeX-view-program-list
                 '("PDF Tools" TeX-pdf-tools-sync-view))))

(use-package cdlatex
  :after auctex)

(provide 'lang-latex)
;;; lang-latex.el ends here
