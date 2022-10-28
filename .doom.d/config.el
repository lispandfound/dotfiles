;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Jake Faulkner"
      user-mail-address "jakefaulkn@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!
(setq doom-font (font-spec :family "RobotoMono Nerd Font" :size 14))
(setq doom-variable-pitch-font (font-spec :family "Roboto" :size 14 :weight 'semi-light))
(setq doom-unicode-font (font-spec :family "Noto Sans Math"))
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'modus-operandi)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!


(after! LaTeX
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-electric-math (cons "\\(" "\\)")
        LaTeX-electric-left-right-brace t
        TeX-electric-sub-and-superscript t
        TeX-command-extra-options "-shell-escape"
        TeX-master nil
        TeX-source-correlate-start-server t
        TeX-engine 'xetex)
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
  (add-hook 'latex-mode-hook #'turn-on-auto-fill)
  (LaTeX-add-environments '("theorem"  LaTeX-env-label)
                          '("lemma" LaTeX-env-label)
                          '("definition" LaTeX-env-label)
                          '("corollary" LaTeX-env-label))
  (setf LaTeX-label-alist (cl-list* '("lemma" . "lem:") '("theorem" . "thm:") '("definition" . "def:") '("corollary" . "cor:") LaTeX-label-alist)))


(after! reftex
  (add-to-list 'reftex-label-alist '("theorem" ?h "thm:" "~\\ref{%s}" t ("Theorem" "theorem") nil) )
  (add-to-list 'reftex-label-alist '("definition" ?d "def:" "~\\ref{%s}" t ("Definition" "definition") nil) )
  (add-to-list 'reftex-label-alist '("corollary" ?c "cor:" "~\\ref{%s}" t ("Corollary" "corollary") nil) )
  (add-to-list 'reftex-label-alist '("lemma" ?m "lem:" "~\\ref{%s}" t ("Lemma" "lemma") nil) ))


(after! cdlatex
  (add-hook 'cdlatex-mode-hook
            (lambda () (when (eq major-mode 'org-mode)
                    (make-local-variable 'org-pretty-entities-include-sub-superscripts)
                    (setq org-pretty-entities-include-sub-superscripts nil))))
  (defun add-labelled-env (environment shortcut)
    (add-to-list 'cdlatex-env-alist (list environment (format "\\begin{%s}\nAUTOLABEL\n?\n\\end{%s}" environment environment) nil))
    (add-to-list 'cdlatex-command-alist (list shortcut (format "Insert %s env" environment) "" 'cdlatex-environment (list environment) t nil)))
  (defun jake/cdlatex-hook ()
    ;; Fixing #35 on github, cdlatex-takeover-parenthesis doesn't work...
    (unbind-key "(" cdlatex-mode-map)
    (unbind-key "{" cdlatex-mode-map)
    (unbind-key "[" cdlatex-mode-map))
  (add-to-list 'safe-local-variable-values
               '(TeX-command-extra-options . "-shell-escape"))
  (map! :map cdlatex-mode-map
        "TAB" #'cdlatex-tab)
  (add-hook! LaTeX-mode #'jake/cdlatex-hook #'+word-wrap-mode)

  (setq cdlatex-use-dollar-to-ensure-math nil)
  (add-to-list 'cdlatex-math-modify-alist '(115 "\\mathbb" nil t nil nil))
  (dolist (kv '(("theorem" "thm") ("definition" "def") ("corollary" "cor") ("lemma" "lem")))
    (add-labelled-env (car kv) (cadr kv))))




(after! ox-latex
  (add-to-list 'org-latex-classes
              '("book"
                "\\documentclass{book}"
                ("\\chapter{%s}" . "\\chapter*{%s}")
                ("\\section{%s}" . "\\section*{%s}")
                ("\\subsection{%s}" . "\\subsection*{%s}")
                ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
              ))
(setq evil-want-fine-undo t)
(after! (org transient)
  (transient-define-prefix org-element-transient ()
    "Org Mode Element Transient State"
    ["Motion"
     ("h" "Up Element" org-up-element :transient t)
     ("l" "Down Element" org-down-element :transient t)
     ("j" "Forward Element" org-forward-element :transient t)
     ("k" "Backward Element" org-backward-element :transient t)]
    ["Move"
     ("K" "Move Subtree Up" org-move-subtree-up :transient t)
     ("J" "Move Subtree Down" org-move-subtree-down :transient t)
     ("H" "Promote Subtree" org-promote-subtree :transient t)
     ("L" "Demote Subtree" org-demote-subtree :transient t)
     ("r" "Refile Subtree" org-refile :transient t)]
    ["Store"
     ("n" "Store Link" org-store-link :transient t)])
  (map! :map org-mode-map
        :n "g." 'org-element-transient))

(after! org
  (setq org-stuck-projects '("+LEVEL=2+PROJECT" ("TODO") nil ""))
  (setq org-highlight-latex-and-related '(script entities))
  (after! smartparens
    (sp-local-pair 'org-mode "\\[" "\\]")
    (sp-local-pair 'org-mode "$" "$")
    (sp-local-pair 'org-mode "'" "'" :actions '(rem))
    (sp-local-pair 'org-mode "=" "=" :actions '(rem))
    (sp-local-pair 'org-mode "\\left(" "\\right)" :trigger "\\l(" :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair 'org-mode "\\left[" "\\right]" :trigger "\\l[" :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair 'org-mode "\\left\\{" "\\right\\}" :trigger "\\l{" :post-handlers '(sp-latex-insert-spaces-inside-pair))
    (sp-local-pair 'org-mode "\\left|" "\\right|" :trigger "\\l|" :post-handlers '(sp-latex-insert-spaces-inside-pair)))


  (setq org-capture-templates '(("t" "Personal todo" entry
                                 (file+headline +org-capture-todo-file "Inbox")
                                 "* TODO %?\n%i\n%a" :prepend t)
                                ("n" "Personal notes" entry
                                 (file+headline +org-capture-notes-file "Inbox")
                                 "* %u %?\n%i\n%a" :prepend t)))
  (setq org-agenda-files '("~/Sync/todo.org")
        org-refile-targets '((nil . (:maxlevel . 2)) ("~/Sync/archive.org" . (:level . 1)))
        org-default-notes-file "~/Sync/todo.org"
        org-directory "~/Sync/"
        org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "KILL(k)") (sequence "[ ](T)" "[?](W)" "[P](P)" "|" "[X](D)" "[-](K)" ))
        org-pretty-entities t
        org-hide-emphasis-markers t
        org-roam-directory "~/Sync/org-roam"
        org-superstar-headline-bullets-list '(" ")
        org-attach-id-dir ".attach"
        org-ellipsis "  "
        org-agenda-block-separator "")
  (add-hook! org-mode #'evil-tex-mode (bibtex-set-dialect 'biblatex)))


(global-set-key (kbd "M-/") 'hippie-expand)

(setq-default abbrev-mode t)
(add-hook 'prog-mode-hook (lambda () (abbrev-mode -1)))
(setq abbrev-file-name "~/.emacs.d/abbrev.el")
(use-package! company-ctags
  :config (company-ctags-auto-setup))

(after! mu4e
  (require 'smtpmail)
  (setq
   sendmail-program (executable-find "msmtp")
   send-mail-function #'smtpmail-send-it
   message-sendmail-f-is-evil t
   message-sendmail-extra-arguments '("--read-envelope-from")
   message-send-mail-function #'message-send-mail-with-sendmail)
  (set-email-account! "uni"
                      '((mu4e-sent-folder       . "/Sent Mail")
                        (mu4e-drafts-folder     . "/Drafts")
                        (mu4e-trash-folder      . "/Trash")
                        (message-send-mail-function . message-send-mail-with-sendmail)
                        (mail-specify-envelope-from . t)
                        (message-sendmail-envelope-from . header)
                        (mail-envelope-from . header)
                        (mu4e-refile-folder     . "/All Mail")
                        (smtpmail-user-mail-address . "jaf150@uclive.ac.nz")
                        (user-mail-address      . "jake.faulkner@pg.canterbury.ac.nz"))
                      t))

(after! citar
  (setq! citar-bibliography '("~/Sync/bibliography/bibliography.bib")
         citar-library-paths '("~/Sync/bibliography/pdfs")))
(use-package! oxr
  :after org
  :config (setq oxr-types '((figure . "fig")
                            (table . "tab")
                            (equation . "eq")
                            (section . "sec"))))

(use-package gap
  :mode (("\\.g\\'" . gap-mode)
         ("\\.gap\\'" . gap-mode))
  :init
  (setq gap-executable "/usr/bin/gap"
        gap-electric-semicolon nil
        gap-electric-equals nil)
  (add-hook 'gap-mode-hook (lambda () (add-to-list 'company-backends '(company-ctags))))
  (set-docsets! 'gap-mode "gap" "fining")
  (set-docsets! 'gap-process-mode "gap" "fining")
  (defun +gap-open-repl ()
    (interactive)
    (unless (gap-running-p)
      (gap))
    (pop-to-buffer gap-process-buffer))
  (set-repl-handler! 'gap-mode #'+gap-open-repl
    :send-region 'gap-eval-region
    :send-buffer 'gap-eval-buffer)
  (set-company-backend! 'gap-mode 'company-gap-backend)
  (set-popup-rule! "^\\*GAP Help\\*" :size 0.3))

(use-package gap-company
  :commands (company-gap-backend))

(use-package maxima
  :defer t)
(after! biblio
  (setq biblio-crossref-user-email-address "jake.faulkner@pg.canterbury.ac.nz")
  (defun biblio-url-retrieve (url callback)
  "Wrapper around `url-queue-retrieve'.
URL and CALLBACK; see `url-queue-retrieve'"
  (message "Fetching %s" url)
  (if biblio-synchronous
      (with-current-buffer (url-retrieve-synchronously url)
        (funcall callback nil))
    (setq url-queue-timeout 5)
    (url-queue-retrieve url callback))))

(setq-default cursor-type 'bar)

(after! citar
  (setq citar-notes-paths '("~/Sync/org-roam")))

(defun jake/share-this-file ()
  (interactive)
  (let* ((fp (read-file-name "File to share: " (buffer-file-name (current-buffer))))
         (command (format "woof %s" fp))
         (proc (start-process-shell-command "woof" (get-buffer-create "*woof*") command)))
    (set-process-filter proc (lambda (proc line)
                               (when (string-match "Now serving on \\(.*\\)" line)
                                 (message line))))))

(after! avy
  (setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)))

(defadvice he-substitute-string (after he-smartparens-fix)
  "remove extra paren when expanding line in smartparens"
  (if (and smartparens-mode (member (substring str -1) '(")" "]" "}")))
      (save-excursion
        (progn (backward-delete-char 1) (forward-char)))))

(add-to-list '+lookup-provider-url-alist '("MathSciNet" "https://mathscinet.ams.org/mathscinet/search/publications.html?pg4=TI&s4=%s&co4=AND&pg5=DOI&s5=&co5=AND&pg6=PC&s6=&co6=AND&pg7=ALLF&s7=&co7=AND&dr=all&yrop=eq&arg3=&yearRangeFirst=&yearRangeSecond=&pg8=ET&s8=All&review_format=html&Submit=Search"))
(add-to-list '+lookup-provider-url-alist '("GScholar" "https://scholar.google.com/scholar?&q=%s"))
(add-to-list '+lookup-provider-url-alist '("Encyclopedia of Mathematics" "https://encyclopediaofmath.org/index.php?search=%s"))

(setq hippie-expand-try-functions-list '(try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol))
(add-hook! dashboard-mode-hook (setq-local display-line-numbers nil))
(advice-remove #'delete-backward-char #'+default--delete-backward-char-a)
(add-hook 'auto-save-hook 'org-save-all-org-buffers)
(defun delete-other-workspaces ()
  "Delete all workspaces except for the current workspace."
  (interactive)
  (let ((current (+workspace-current)))
    (-map (lambda (wk) (+workspace-delete (persp-name wk))) (delete current (+workspace-list)))))

(map!
 :leader
 "TAB C-o" #'delete-other-workspaces)
(setq bookmark-default-file "~/.doom.d/bookmarks")

(setq evil-ex-substitute-global t)

(defun open-current-directory-sysfm ()
  (interactive)
  (call-process-shell-command (format "xdg-open \"%s\"&" (f-dirname (buffer-file-name)))))

(map! :leader "o." 'open-current-directory-sysfm)

(after! (embark consult org)
  (defun consult-org-store-link (candidate)
    (save-excursion
      (goto-char (get-text-property 0 'consult--candidate candidate))
      (org-store-link nil t)))
  (embark-define-keymap embark-consult-org-heading
    "Consult Org Embark Bindings"
    ("n" consult-org-store-link))
  (add-to-list 'embark-keymap-alist '(consult-org-heading . embark-consult-org-heading)))

(after! (citar)
       (setq citar-symbols
             `((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
               (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
               (link ,(all-the-icons-octicon "link" :face 'all-the-icons-orange :v-adjust 0.01) . " ")))
       (setq citar-symbol-separator "  ") )

(defconst small-words '("a" "an" "and" "as"
                        "at" "but" "by" "en" "for" "if" "of" "on" "or" "the" "to" "v" "v." "via" "vs"
                        "vs."))

(defun titlecase-string (str)
  "Convert string STR to title case and return the resulting string."
  (let ((words (s-split " " str)))
    (s-join " " (cons (s-titleize (car words))
                      (-map (lambda (w) (let ((dw (s-downcase w)))
                                     (if (member dw small-words)
                                         dw
                                       (s-titleize w)))) (cdr words))))))

(defun titlecase-region (begin end)
  "Convert text in region from BEGIN to END to title case."
  (interactive "*r")
  (let ((pt (point)))
    (insert (titlecase-string (delete-and-extract-region begin end)))
    (goto-char pt)))

(defun titlecase-dwim ()
  "Convert the region or current line to title case.
If Transient Mark Mode is on and there is an active region, convert
the region to title case.  Otherwise, work on the current line."
  (interactive)
  (if (and transient-mark-mode mark-active)
      (titlecase-region (region-beginning) (region-end))
    (titlecase-region (point-at-bol) (point-at-eol))))

(use-package! consult-recoll
  :commands (consult-recoll)
  :init
  (map! :leader "sR" 'consult-recoll))
;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
