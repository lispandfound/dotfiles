(tempo-define-template "lambda"
                          '(> "(lambda (" p ")" n> r> ")">)
                          nil
                          "Insert a template for an anonymous procedure")

(define-abbrev emacs-lisp-mode-abbrev-table "lambda" "" 'tempo-template-lambda)
(tempo-define-template "mm" '("\\(" p "\\)") nil "Template for mathematics in latex")
(with-eval-after-load 'org
  (define-abbrev org-mode-abbrev-table "mm" "" 'tempo-template-mm))
