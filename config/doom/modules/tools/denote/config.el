;;; tools/denote/config.el -*- lexical-binding: t; -*-


(use-package! denote
  :hook (dired-mode . denote-dired-mode)
  :custom
  (denote-directory (expand-file-name "~/notes/"))
  :config
  ;; Automatically rename Denote buffers when opening them so that
  ;; instead of their long file name they have, for example, a literal
  ;; "[D]" followed by the file's title.  Read the doc string of
  ;; `denote-rename-buffer-format' for how to modify this.
  (denote-rename-buffer-mode 1))

(map! :leader
      :prefix ("d" . "denote")
      :desc "New note"                "d" #'denote
      :desc "Journal entry"           "j" #'denote-journal-new-or-existing-entry
      :desc "Open or create"          "n" #'denote-open-or-create
      :desc "New with date"           "t" #'denote-date
      :desc "Create in subdirectory"  "+" #'denote-subdirectory

      :prefix ("db" . "bookkeeping")
      :desc "Rename"                  "r" #'denote-rename-file
      :desc "Rename frontmatter"      "f" #'denote-rename-file-using-front-matter
      :desc "Modify keywords"         "k" #'denote-rename-file-keywords

      :prefix ("dl" . "linking")
      :desc "Insert link"             "i" #'denote-link
      :desc "Link to heading"         "h" #'denote-org-link-to-heading
      :desc "Backlinks"               "b" #'denote-backlinks
      :desc "Find backlink"           "g" #'denote-find-backlink
      :desc "Org backlink block"      "o" #'denote-org-dblock-insert-backlinks

      :prefix ("ds" . "searching")
      :desc "Consult notes"           "n" #'consult-notes
      :desc "Search all notes"        "s" #'consult-notes-search-in-all-notes)

(use-package! denote-review)
(use-package! denote-journal
  :after denote
  :custom
  (denote-journal-keyword "labnotes")
  (denote-journal-directory (expand-file-name "~/notes/labnotes"))
  (denote-journal-title-format 'day-date-month-year))
(use-package! denote-org
  :after denote
  :custom
  ;; `denote-org-link-to-heading' controls the behavior of
  ;; `org-store-link': setting to id makes it insert an ID in the
  ;; PROPERTIES drawer
  ;;
  ;; (denote-org-store-link-to-heading 'context)
  (denote-org-store-link-to-heading 'id))

(use-package! consult-notes
  :commands (consult-notes consult-notes-search-in-all-notes)
  :config
  ;; Set org-roam integration, denote integration, or org-heading integration e.g.:
  (consult-notes-org-headings-mode)
  (consult-notes-denote-mode)
  ;; search only for text files in denote dir
  (setopt consult-notes-denote-files-function (lambda () (denote-directory-files nil t t))))


(use-package org-fragtog
  :ensure t
  :hook (org-mode . org-fragtog-mode))
