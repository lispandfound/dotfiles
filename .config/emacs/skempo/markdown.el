(skempo-define-skeleton img (:tag t :abbrev t :mode (markdown-mode))
  nil
  "![" (skeleton-read "Alt Text: ") "](" (skeleton-read "Image: ") ")"
  )

(skempo-define-tempo alert (:tag t :abbrev t :mode (markdown-mode))
  nil
  "> [!NOTE]" n> r>
  ">" p
  )
