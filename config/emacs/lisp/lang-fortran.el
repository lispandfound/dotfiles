;;; lang-fortran.el --- Fortran configuration -*- lexical-binding: t; -*-

;; f90-mode and fortran-mode are built-in.
(use-package f90
  :ensure nil
  :hook (f90-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '(f90-mode . ("fortls" "--lowercase_intrinsics"))))

(use-package fortran
  :ensure nil
  :hook (fortran-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '(fortran-mode . ("fortls" "--lowercase_intrinsics"))))

(provide 'lang-fortran)
;;; lang-fortran.el ends here
