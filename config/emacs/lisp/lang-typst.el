;;; lang-typst.el --- Typst configuration -*- lexical-binding: t; -*-

;; Drafting happens in Typst; LaTeX is an export target for coworkers and
;; journal submission.  See `my/typst-export-latex' and the pandoc filter in
;; ../pandoc/typst-to-latex.lua for that half.

(defgroup my/typst nil
  "Typst authoring with a LaTeX export path."
  :group 'languages
  :prefix "my/typst-")

;;; =========================================================================
;;; MAJOR MODE + LSP
;;; =========================================================================

;; typst-ts-mode is not on MELPA.  Its tree-sitter grammar is installed once
;; with `M-x typst-ts-mc-install-grammar' -- treesit-auto doesn't cover Typst.
(use-package typst-ts-mode
  :ensure (:host codeberg :repo "meow_king/typst-ts-mode" :branch "main")
  :mode ("\\.typ\\'" . typst-ts-mode)
  :hook ((typst-ts-mode . eglot-ensure)
         (typst-ts-mode . visual-line-mode))
  :custom
  (typst-ts-mode-enable-raw-blocks-highlight t)
  (typst-ts-watch-options "--open")
  :config
  (add-to-list 'eglot-server-programs
               '((typst-ts-mode) . ("tinymist" "lsp")))

  ;; Formatting belongs to apheleia via typstyle (below), so tinymist's own
  ;; formatter is off -- the same split as ruff-lsp in lang-python.el.
  ;; exportPdf is "never" because the watch-mode/pdf-tools loop owns PDF
  ;; output; letting the server export too would fight it.
  (add-hook 'typst-ts-mode-hook
            (lambda ()
              (setq-local eglot-workspace-configuration
                          '(:tinymist (:formatterMode "disable"
                                       :exportPdf "never"
                                       :lint (:enabled t :when "onType")
                                       ;; Resolve multi-file documents through
                                       ;; typst.toml rather than treating each
                                       ;; chapter as a standalone file.
                                       :projectResolution "lockDatabase")))))

  (keymap-set typst-ts-mode-map "C-c C-c" #'typst-ts-tmenu)
  (keymap-set typst-ts-mode-map "C-c C-e" #'my/typst-export-latex)
  (keymap-set typst-ts-mode-map "C-c C-i" #'my/typst-import-latex))

;;; =========================================================================
;;; FORMATTING — typstyle via apheleia
;;; =========================================================================

;; With no file argument and no -i, typstyle reads stdin and writes stdout,
;; which is apheleia's calling convention.
(with-eval-after-load 'apheleia
  (setf (alist-get 'typstyle apheleia-formatters) '("typstyle"))
  (setf (alist-get 'typst-ts-mode apheleia-mode-alist) 'typstyle))

;;; =========================================================================
;;; COMPILATION NAVIGATION
;;; =========================================================================

;; `typst compile' emits codespan-style diagnostics: a severity line, then an
;; indented location line.  Unicode box-drawing is used when the terminal
;; supports it and "-->" otherwise, so both are matched.
;;
;;   error: expected expression
;;     ┌─ bad.typ:5:8
;;
;; Prepended so it wins over the generic `gnu' rule, as with the ruff/ty
;; rules in lang-python.el.
(with-eval-after-load 'compile
  (setf (alist-get 'typst compilation-error-regexp-alist-alist)
        `(,(concat "^\\(?:error\\|\\(?4:warning\\)\\): .*\n"
                   " *\\(?:┌─\\|-->\\) "
                   "\\(?1:.+?\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\)$")
          1 2 3 (4 . nil)))
  (add-to-list 'compilation-error-regexp-alist 'typst))

;;; =========================================================================
;;; LATEX INTEROP
;;; =========================================================================

(defcustom my/typst-latex-citation-style 'biblatex
  "Citation mode `my/typst-export-latex' passes to pandoc.
`biblatex' renders \\autocite, `natbib' renders \\citep, and `citeproc'
resolves citations into a static bibliography needing no .bib file at
the far end.  A prefix argument overrides this per invocation.

Compiling biblatex output needs biber (pacman -S biber); natbib output
builds with the bibtex that ships with TeX Live.  Cross-references
resolve either way -- only the bibliography depends on this."
  :type '(choice (const biblatex) (const natbib) (const citeproc))
  :group 'my/typst)

(defcustom my/typst-latex-standalone t
  "When non-nil, export a full LaTeX document rather than a body fragment.
Nil is the better choice when the output is destined to be pasted into a
journal template that supplies its own preamble."
  :type 'boolean
  :group 'my/typst)

(defcustom my/typst-latex-number-sections t
  "When non-nil, pass --number-sections to pandoc.
Pandoc otherwise emits unnumbered \\section*, which leaves a section
cross-reference with no number to resolve to -- \\Cref{sec-intro} renders
as \"??\".  Numbered sections are also what journal classes expect."
  :type 'boolean
  :group 'my/typst)

(defun my/typst--lua-filter ()
  "Absolute path to the Typst-to-LaTeX pandoc filter."
  (expand-file-name "pandoc/typst-to-latex.lua" user-emacs-directory))

(defun my/typst--citation-flag (style)
  "Return the pandoc flag selecting citation STYLE."
  (pcase style
    ('biblatex "--biblatex")
    ('natbib   "--natbib")
    ('citeproc "--citeproc")
    (_ (user-error "Unknown citation style: %s" style))))

(defun my/typst--read-citation-style ()
  "Prompt for a citation style, defaulting to `my/typst-latex-citation-style'."
  (intern (completing-read
           (format "Citation style (default %s): " my/typst-latex-citation-style)
           '("biblatex" "natbib" "citeproc") nil t nil nil
           (symbol-name my/typst-latex-citation-style))))

(defun my/typst--convert (command output)
  "Run COMMAND in a compilation buffer, visiting OUTPUT if it succeeds.
Errors land in *pandoc* rather than a minibuffer message, so a failed
conversion can be read in full."
  (let ((buffer (compilation-start command nil (lambda (_) "*pandoc*"))))
    ;; `compilation-finish-functions' is global, hence the buffer guard and
    ;; the self-removal: this closure is for this one conversion only.
    (letrec ((finish
              (lambda (buf status)
                (when (eq buf buffer)
                  (remove-hook 'compilation-finish-functions finish)
                  (if (string-prefix-p "finished" status)
                      (progn (message "Wrote %s" output)
                             (find-file-other-window output))
                    (message "pandoc failed: %s" (string-trim status)))))))
      (add-hook 'compilation-finish-functions finish))
    buffer))

;;;###autoload
(defun my/typst-export-latex (&optional arg)
  "Export the current Typst buffer to a LaTeX file beside it.

Conversion goes through pandoc, whose Typst reader handles prose, math
and citations but mishandles cross-references and labels; the Lua filter
at `my/typst--lua-filter' repairs those.  Citation style comes from
`my/typst-latex-citation-style'; with prefix ARG, prompt for it instead.

Note that Typst templates and #show rules have no LaTeX equivalent and
are dropped -- keep content in plain markup for a clean handoff."
  (interactive "P")
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file"))
  (when (buffer-modified-p)
    (save-buffer))
  (let* ((style (if arg (my/typst--read-citation-style)
                  my/typst-latex-citation-style))
         (input buffer-file-name)
         (output (concat (file-name-sans-extension input) ".tex"))
         (command (mapconcat
                   #'identity
                   (append
                    (list "pandoc" "-f" "typst" "-t" "latex")
                    (when my/typst-latex-standalone '("--standalone"))
                    (when my/typst-latex-number-sections '("--number-sections"))
                    ;; The filter must precede the citation flag: pandoc runs
                    ;; filters in command-line order, and under --citeproc the
                    ;; filter has to install the bibliography metadata before
                    ;; citeproc looks for it, or every citation resolves to
                    ;; "knuth1984?".  Harmless for --biblatex/--natbib.
                    (list (concat "--lua-filter="
                                  (shell-quote-argument (my/typst--lua-filter)))
                          (my/typst--citation-flag style)
                          "-o" (shell-quote-argument output)
                          (shell-quote-argument input)))
                   " ")))
    (my/typst--convert command output)))

;;;###autoload
(defun my/typst-import-latex (file)
  "Convert a LaTeX FILE to Typst, writing the result beside it.

The inverse of `my/typst-export-latex', for pulling a coworker's .tex
back in.  Prose, headings, math and citations survive; cross-references
come back as #link rather than @ref and need a manual pass if the file
will be exported again."
  (interactive
   (list (read-file-name
          "LaTeX file: " nil nil t
          (when (and buffer-file-name
                     (string= (file-name-extension buffer-file-name) "tex"))
            (file-name-nondirectory buffer-file-name)))))
  (let* ((output (concat (file-name-sans-extension (expand-file-name file))
                         ".typ"))
         (command (mapconcat
                   #'shell-quote-argument
                   (list "pandoc" "-f" "latex" "-t" "typst"
                         "-o" output (expand-file-name file))
                   " ")))
    (my/typst--convert command output)))

(provide 'lang-typst)
;;; lang-typst.el ends here
