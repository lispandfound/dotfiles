(skempo-define-skeleton img (:tag t :abbrev t :mode (markdown-mode))
  nil
  "![" (skeleton-read "Alt Text: ") "](" (skeleton-read "Image: ") ")"
  )

(skempo-define-tempo alert (:tag t :abbrev t :mode (markdown-mode))
  nil
  "> [!NOTE]" n> r>
  ">" p
  )


(skempo-define-skeleton wstage (:tag t :abbrev t :mode (markdown-mode))
  nil
  "## " (skeleton-read "Stage Name: ") \n
  "### Description" \n (skeleton-read "Stage Description: ") \n
  "### Inputs" \n \n
  "### Outputs" \n \n
  "### Environment " \n \n
  "### Usage" \n \n
  "### For More Help"\n "See the output of `" (skeleton-read "Stage Executable: ") " --help` or")
