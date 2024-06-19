(skempo-define-tempo def (:tag t :abbrev t :mode (python-mode))
  "def " p "(" p "):" n> r>)

(skempo-define-tempo if (:tag t :abbrev t :mode (python-mode))
  "if " p ":" n> r>)

(skempo-define-tempo class (:tag t :abbrev t :mode (python-mode))
  "class " p ":" n> r>)

(skempo-define-tempo for (:tag t :abbrev t :mode (python-mode))
  "for " p ":" n> r>)
