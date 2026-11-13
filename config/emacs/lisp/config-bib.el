;;; config-bib.el --- Bibliography: citar, biblio, OpenAlex -*- lexical-binding: t; -*-

;; Two halves:
;;   - citar inserts citations for keys already in the document's .bib file.
;;   - biblio searches OpenAlex (backend defined below -- biblio ships none),
;;     and `a' in its results buffer appends the entry to the document's .bib
;;     file and drops @key at point in the buffer the search started from.
;;
;; The .bib file is whatever the buffer's #bibliography(...) call names, so
;; there is no global `citar-bibliography' to keep in sync per paper.
;;
;; The OpenAlex API key comes from auth-source:
;;   machine api.openalex.org password <key>
;; Without one, requests share OpenAlex's small anonymous daily budget.

(require 'bibtex)       ; its autokey variables are let-bound below
(require 'thingatpt)
(require 'auth-source)

(defvar biblio-backends)
(defvar biblio--target-buffer)

(defgroup my/bib nil
  "Bibliography search and citation."
  :group 'tools
  :prefix "my/bib-")

;;; =========================================================================
;;; TYPST BIBLIOGRAPHY DISCOVERY
;;; =========================================================================

(defun my/bib-typst-files ()
  "Return absolute paths of the .bib files this Typst buffer uses.
Read from the buffer's #bibliography(...) call; a leading \"/\" is the
project root, as in Typst.  Falls back to the .bib files beside the
buffer's file, which covers chapters of a document whose
#bibliography call lives in main.typ."
  (let* ((dir (if buffer-file-name
                  (file-name-directory buffer-file-name)
                default-directory))
         (root (if-let* ((pr (project-current))) (project-root pr) dir))
         files)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "#bibliography(" nil t)
        (let ((end (save-excursion (re-search-forward ")\\s-*$" nil t))))
          ;; Every string literal in the call; the .bib filter drops the
          ;; style: "x.csl" argument and any Hayagriva .yml files.
          (while (re-search-forward "\"\\([^\"]+\\)\"" end t)
            (let ((name (match-string-no-properties 1)))
              (when (string-suffix-p ".bib" name)
                (push (if (string-prefix-p "/" name)
                          (expand-file-name (substring name 1) root)
                        (expand-file-name name dir))
                      files)))))))
    (or (nreverse files)
        (directory-files dir t "\\.bib\\'"))))

(defun my/bib-typst-insert-keys (keys)
  "Insert KEYS as Typst citations at point.
Keys that Typst's @ shorthand can't carry (e.g. ending in \".\") go
through #cite(label(...)) instead."
  (insert
   (mapconcat (lambda (key)
                (if (string-match-p "\\`[[:alnum:]_:.-]*[[:alnum:]_]\\'" key)
                    (concat "@" key)
                  (format "#cite(label(%S))" key)))
              keys " ")))

(defun my/bib-typst-insert-citation (keys &optional _invert)
  "Insert a Typst citation for KEYS, space-separating from preceding text."
  (unless (or (bolp) (memq (char-before) '(?\s ?\t ?\( ?\[)))
    (insert " "))
  (my/bib-typst-insert-keys keys))

(defun my/bib-typst-insert-prose-citation (keys)
  "Insert KEYS as Typst prose citations, e.g. \"Boore (1972)\".
Bound to `p' in citar's Embark map, so it acts on the references
selected in `citar-insert-citation'."
  (interactive (list (citar-select-refs)))
  (unless (derived-mode-p 'typst-ts-mode)
    (user-error "Prose citations are only defined for Typst buffers"))
  (unless (or (bolp) (memq (char-before) '(?\s ?\t ?\( ?\[)))
    (insert " "))
  (insert
   (mapconcat (lambda (key)
                (format "#cite(%s, form: \"prose\")"
                        (if (string-match-p "\\`[[:alnum:]_:.-]*[[:alnum:]_]\\'" key)
                            (format "<%s>" key)
                          (format "label(%S)" key))))
              keys ", ")))

(defun my/bib-typst-key-at-point ()
  "Return the @key citation at point, or nil."
  (when (thing-at-point-looking-at "@\\([[:alnum:]_:.-]*[[:alnum:]_]\\)")
    (match-string-no-properties 1)))

;;; =========================================================================
;;; CITAR
;;; =========================================================================

(use-package citar
  :hook (typst-ts-mode . citar-capf-setup)
  :config
  ;; Prepended, so it wins over the catch-all `t' entry at the end.
  (add-to-list 'citar-major-mode-functions
               '((typst-ts-mode) .
                 ((local-bib-files . my/bib-typst-files)
                  (insert-citation . my/bib-typst-insert-citation)
                  (insert-keys . my/bib-typst-insert-keys)
                  (key-at-point . my/bib-typst-key-at-point)))))

(use-package citar-embark
  :after (citar embark)
  :config
  (citar-embark-mode 1)
  ;; Multitarget, so every reference selected in the minibuffer is cited.
  (keymap-set citar-embark-map "p" #'my/bib-typst-insert-prose-citation)
  (add-to-list 'embark-multitarget-actions #'my/bib-typst-insert-prose-citation))

;;; =========================================================================
;;; OPENALEX BACKEND FOR BIBLIO
;;; =========================================================================

(defun my/bib-openalex--api-key ()
  "Return the OpenAlex API key from auth-source, or nil."
  (auth-source-pick-first-password :host "api.openalex.org"))

(defun my/bib-openalex--url (query)
  "Return the OpenAlex works-search URL for QUERY."
  (let ((key (my/bib-openalex--api-key)))
    (concat "https://api.openalex.org/works?per_page=25"
            "&select=id,doi,display_name,publication_year,authorships,"
            "primary_location,type,biblio"
            "&search=" (url-hexify-string query)
            (if key (concat "&api_key=" (url-hexify-string key)) ""))))

(defun my/bib-openalex--extract (work)
  "Convert OpenAlex WORK into biblio's metadata alist."
  (let-alist work
    (let ((doi (and .doi (string-remove-prefix "https://doi.org/" .doi))))
      (list (cons 'doi doi)
            (cons 'year (and (numberp .publication_year)
                             (number-to-string .publication_year)))
            (cons 'title .display_name)
            (cons 'authors (seq-map (lambda (a) (let-alist a .author.display_name))
                                    .authorships))
            (cons 'container .primary_location.source.display_name)
            (cons 'type .type)
            (cons 'references (delq nil (list doi)))
            (cons 'url (or .doi .id))
            ;; Not displayed; kept for `my/bib-openalex--bibtex-from-metadata'.
            (cons 'volume .biblio.volume)
            (cons 'issue .biblio.issue)
            (cons 'pages (and .biblio.first_page
                              (if .biblio.last_page
                                  (concat .biblio.first_page "--" .biblio.last_page)
                                .biblio.first_page)))))))

(defun my/bib-openalex--parse-search-results ()
  "Extract search results from an OpenAlex response."
  (biblio-decode-url-buffer 'utf-8)
  (let-alist (json-read)
    (seq-map #'my/bib-openalex--extract .results)))

(defun my/bib-openalex--bibtex-from-metadata (metadata)
  "Build a BibTeX entry from OpenAlex METADATA.
Used for works with no DOI, or whose DOI resolver offers no BibTeX.
The key is a placeholder; `my/bib--add-and-cite' generates the real one."
  (let-alist metadata
    (let ((article (and (equal .type "article") .container)))
      (concat
       (format "@%s{openalex,\n" (if article "article" "misc"))
       (mapconcat
        (pcase-lambda (`(,field . ,value)) (format "  %s = {%s}" field value))
        (seq-filter #'cdr
                    `(("author" . ,(and .authors (string-join .authors " and ")))
                      ("title" . ,.title)
                      (,(if article "journal" "howpublished") . ,.container)
                      ("year" . ,.year)
                      ("volume" . ,.volume)
                      ("number" . ,.issue)
                      ("pages" . ,.pages)
                      ("doi" . ,.doi)
                      ("url" . ,.url)))
        ",\n")
       "\n}\n"))))

(defun my/bib-openalex--forward-bibtex (metadata forward-to)
  "Pass BibTeX for OpenAlex METADATA to FORWARD-TO.
Prefer the DOI resolver's BibTeX, which is richer than OpenAlex's own
metadata; fall back to building an entry from the search result."
  (if-let* ((doi (biblio-alist-get 'doi metadata)))
      (biblio-doi-forward-bibtex
       doi (lambda (bibtex)
             (funcall forward-to
                      (or bibtex (my/bib-openalex--bibtex-from-metadata metadata)))))
    (funcall forward-to (my/bib-openalex--bibtex-from-metadata metadata))))

(defun my/bib-openalex-backend (command &optional arg &rest more)
  "An OpenAlex backend for biblio.el.
COMMAND, ARG, MORE: See `biblio-backends'."
  (pcase command
    ('name "OpenAlex")
    ('prompt "OpenAlex query: ")
    ('url (my/bib-openalex--url arg))
    ('parse-buffer (my/bib-openalex--parse-search-results))
    ('forward-bibtex (my/bib-openalex--forward-bibtex arg (car more)))
    ('register (add-to-list 'biblio-backends #'my/bib-openalex-backend))))

;;; =========================================================================
;;; ADD TO .BIB AND CITE
;;; =========================================================================

(defun my/bib--autokey ()
  "Generate an author+year key (\"boore1972\") for the entry at point."
  (let ((bibtex-autokey-names 1)
        (bibtex-autokey-names-stretch 0)
        (bibtex-autokey-name-year-separator "")
        (bibtex-autokey-year-length 4)
        (bibtex-autokey-titlewords 0)
        (bibtex-autokey-titlewords-stretch 0)
        (bibtex-autokey-year-title-separator ""))
    (bibtex-generate-autokey)))

(defun my/bib--existing-key-for-doi (doi)
  "Return the key of the entry in the current .bib buffer with DOI, or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (when (re-search-forward
             (concat "^\\s-*doi\\s-*=\\s-*[{\"]\\s-*\\(https?://doi\\.org/\\)?"
                     (regexp-quote doi) "\\s-*[}\"]")
             nil t)
        (bibtex-beginning-of-entry)
        (bibtex-key-in-head)))))

(defun my/bib--unique-key (key)
  "Return KEY, suffixed a, b, ... if the current .bib buffer already has it."
  (let ((candidate key) (suffix ?a))
    (while (save-excursion (bibtex-search-entry candidate))
      (setq candidate (format "%s%c" key suffix)
            suffix (1+ suffix)))
    candidate))

(defun my/bib--add-and-cite (bibtex target)
  "Append BIBTEX to TARGET's .bib file unless present, then cite it in TARGET."
  (let* ((bib-file (with-current-buffer target
                     (or (car (my/bib-typst-files))
                         (read-file-name "Add to .bib file: " nil nil nil nil
                                         (lambda (f) (or (file-directory-p f)
                                                         (string-suffix-p ".bib" f)))))))
         (key
          (with-temp-buffer
            (bibtex-mode)
            (insert bibtex)
            (goto-char (point-min))
            (bibtex-skip-to-valid-entry)
            (let ((doi (bibtex-text-in-field "doi"))
                  (autokey (my/bib--autokey)))
              (with-current-buffer (find-file-noselect bib-file)
                (unless (derived-mode-p 'bibtex-mode) (bibtex-mode))
                (or (and doi (my/bib--existing-key-for-doi doi))
                    (let ((key (my/bib--unique-key autokey)))
                      (with-temp-buffer
                        (insert bibtex)
                        (goto-char (point-min))
                        (re-search-forward "@\\w+\\s-*{\\s-*\\([^,]*\\)," nil t)
                        (replace-match key t t nil 1)
                        ;; DOI resolvers emit bare month names (month = July)
                        ;; that Typst's BibLaTeX parser rejects, and a
                        ;; publisher that CSL styles print after the journal
                        ;; ("Wiley..").  Citations need neither.
                        (while (re-search-forward
                                "^\\s-*\\(month\\|publisher\\)\\s-*=.*\n" nil t)
                          (replace-match ""))
                        (setq bibtex (string-trim (buffer-string))))
                      (save-excursion
                        (goto-char (point-max))
                        (skip-chars-backward " \t\n")
                        (delete-region (point) (point-max))
                        (insert (if (bobp) "" "\n\n") bibtex "\n"))
                      (save-buffer)
                      (message "Added %s to %s" key (file-name-nondirectory bib-file))
                      key)))))))
    (with-selected-window (or (get-buffer-window target) (selected-window))
      (with-current-buffer target
        (if (derived-mode-p 'typst-ts-mode)
            (my/bib-typst-insert-citation (list key))
          (citar-insert-citation (list key)))))))

(defun my/bib-selection-add-and-cite ()
  "Add the result at point to the source buffer's .bib file and cite it."
  (interactive)
  (let ((target biblio--target-buffer))
    (biblio--selection-forward-bibtex
     (lambda (bibtex _metadata) (my/bib--add-and-cite bibtex target))
     t)))

;;;###autoload
(defun my/bib-openalex-lookup (&optional query)
  "Search OpenAlex for QUERY; `a' on a result adds it to the .bib and cites it."
  (interactive)
  (require 'biblio)
  (biblio-lookup #'my/bib-openalex-backend query))

;;;###autoload
(defun my/bib-cite-doi (doi)
  "Fetch BibTeX for DOI, add it to the .bib file and cite it at point."
  (interactive "sDOI: ")
  (require 'biblio)
  (let ((target (current-buffer)))
    (biblio-doi-forward-bibtex
     (biblio-cleanup-doi doi)
     (lambda (bibtex)
       (unless bibtex (user-error "No BibTeX found for %s" doi))
       (my/bib--add-and-cite (biblio-format-bibtex bibtex) target)))))

;; Loaded by the commands above rather than by autoload: `biblio-lookup' is
;; autoloaded from biblio-core, which would skip this :config.
(use-package biblio
  :defer t
  :config
  (keymap-set biblio-selection-mode-map "a" #'my/bib-selection-add-and-cite)
  (add-hook 'biblio-init-hook #'my/bib-openalex-backend))

(provide 'config-bib)
;;; config-bib.el ends here
