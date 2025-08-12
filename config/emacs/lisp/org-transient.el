;;; org-transient.el --- Invoke org agenda via transient -*- lexical-binding: t; -*-
(require 'transient)

(transient-define-prefix org-agenda-transient ()
  "Replace the org-agenda buffer by a transient."
  [["Built-in agendas"
    ("a" "Current day/week" (lambda () (interactive) (org-agenda nil "a")))
    ("t" "Global todo list" (lambda () (interactive) (org-agenda nil "t")))
    ("T" "Global todo list + choose" (lambda () (interactive) (org-agenda nil "T")))
    ("m" "Search tags" (lambda () (interactive) (org-agenda nil "m")))
    ("M" "Search tags with TODO" (lambda () (interactive) (org-agenda nil "M")))
    ("e" "Export" (lambda () (interactive) (org-agenda nil "e")))
    ("s" "Search" (lambda () (interactive) (org-agenda nil "s")))
    ("S" "Search with TODO" (lambda () (interactive) (org-agenda nil "S")))
    ("/" "Multi-occur" (lambda () (interactive) (org-agenda nil "/")))
    ("<" "Restrict" (lambda () (interactive) (org-agenda nil "<")))
    (">" "Remove restriction" (lambda () (interactive) (org-agenda nil ">")))
    ("#" "List stuck projects" (lambda () (interactive) (org-agenda nil "#")))
    ("!" "Define stuck" (lambda () (interactive) (org-agenda nil "!")))
    ("C" "Configure custom agenda views" (lambda () (interactive) (org-agenda nil "C")))]
   ["Custom agendas"
    ("A" "Daily and overview" (lambda () (interactive) (org-agenda nil "A")))
    ("H" "Habits tracker" (lambda () (interactive) (org-agenda nil "H")))]])

(transient-define-prefix org-capture-transient ()
  "Org Capture Templates"
  ["Capture"
   ("l" "Logged completed task" (lambda () (interactive) (org-capture nil "l")))
   ("n" "Note" (lambda () (interactive) (org-capture nil "n")))
   ("t" "Todo" (lambda () (interactive) (org-capture nil "t")))])


(provide 'org-transient)
;;; lookup.el ends here
