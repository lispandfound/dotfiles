;;; lang/csv/config.el -*- lexical-binding: t; -*-


(use-package! csv-mode
  :hook (csv-mode . csv-align-mode))
