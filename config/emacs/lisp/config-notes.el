;;; config-notes.el --- Denote notes git sync + LaTeX to Typst -*- lexical-binding: t; -*-

;; Set-and-forget sync for the Denote notes repository:
;;   - auto-commit the whole notes repo on save,
;;   - pull (and push) on startup,
;;   - one-shot LaTeX math -> Typst fragment conversion for org notes.

(defgroup my/notes nil
  "Git sync for the Denote notes repository."
  :prefix "my/notes-"
  :group 'tools)

(defcustom my/notes-auto-commit t
  "When non-nil, commit the notes repo after any save inside it."
  :type 'boolean
  :group 'my/notes)

(defcustom my/notes-pull-on-startup t
  "When non-nil, pull the notes repo a few seconds after startup."
  :type 'boolean
  :group 'my/notes)

(defcustom my/notes-push-after-pull t
  "When non-nil, push local commits back after a successful startup pull.
Local auto-commits would otherwise never leave this machine."
  :type 'boolean
  :group 'my/notes)

(defcustom my/notes-git-connect-timeout 8
  "ConnectTimeout (seconds) for the ssh used by background git sync.
BatchMode stops an unreachable or password-prompting host from hanging
Emacs; this bounds how long a slow host may block."
  :type 'integer
  :group 'my/notes)

;;; =========================================================================
;;; HELPERS
;;; =========================================================================

(defun my/notes--dir ()
  "Absolute root of the notes repository."
  (expand-file-name
   (if (and (boundp 'denote-directory) (stringp denote-directory))
       denote-directory
     "~/notes/")))

(defun my/notes--repo-p ()
  "Return non-nil when the notes directory is a git repository."
  (file-exists-p (expand-file-name ".git" (my/notes--dir))))

(defun my/notes--git-env ()
  "Environment additions so background git never prompts or hangs."
  (list (format "GIT_SSH_COMMAND=ssh -o BatchMode=yes -o ConnectTimeout=%d"
                my/notes-git-connect-timeout)
        "GIT_TERMINAL_PROMPT=0"))

(defun my/notes--git (&rest args)
  "Run git with ARGS in the notes repo, returning (EXIT . OUTPUT).
The git environment is hardened so it cannot prompt or hang."
  (with-temp-buffer
    (let ((process-environment (append (my/notes--git-env) process-environment)))
      (cons (apply #'process-file "git" nil t nil
                   (append (list "-C" (my/notes--dir)) args))
            (buffer-string)))))

;;; =========================================================================
;;; AUTO-COMMIT ON SAVE
;;; =========================================================================

(defun my/notes--commit-message (file)
  "A commit message for a saved notes FILE."
  (let ((title
         (ignore-errors
           (when (require 'denote nil t)
             (denote-retrieve-front-matter-title-value file)))))
    (format "notes: %s" (or title (file-name-nondirectory file)))))

(defun my/notes-after-save ()
  "Commit the notes repository after saving a buffer inside it."
  (when (and my/notes-auto-commit
             buffer-file-name
             (my/notes--repo-p)
             (not (file-remote-p buffer-file-name)))
    (let ((file (expand-file-name buffer-file-name)))
      (when (string-prefix-p (file-name-as-directory (my/notes--dir)) file)
        (my/notes--git "add" "-A")
        (let ((porcelain (cdr (my/notes--git "status" "--porcelain"))))
          (when (and (stringp porcelain) (string-match-p "[^\n]" porcelain))
            (let ((res (my/notes--git "commit" "-m" (my/notes--commit-message file))))
              (message "notes: %s"
                       (if (= (car res) 0)
                           "committed"
                         (format "commit failed: %s" (cdr res)))))))))))

(add-hook 'after-save-hook #'my/notes-after-save)

;;; =========================================================================
;;; STARTUP PULL / PUSH
;;; =========================================================================

(defvar my/notes--syncing nil
  "Non-nil while an asynchronous notes git sync is in progress.")

(defun my/notes--run-git-async (args cont)
  "Run git ARGS asynchronously in the notes repo.
Call CONT with the exit status and output once it finishes."
  (let* ((buf (generate-new-buffer " *my-notes-git*"))
         (process-environment (append (my/notes--git-env) process-environment))
         (proc (make-process
                :name "my-notes-git"
                :buffer buf
                :command (append (list "git" "-C" (my/notes--dir)) args)
                :noquery t
                :sentinel
                (lambda (p _event)
                  (when (memq (process-status p) '(exit signal))
                    (let ((status (process-exit-status p))
                          (output (with-current-buffer buf (buffer-string))))
                      (when (buffer-live-p buf) (kill-buffer buf))
                      (funcall cont status output)))))))
    proc))

(defun my/notes-sync ()
  "Pull the notes repo; then push local commits back (best effort).

Runs asynchronously and never prompts: unreachable hosts fail fast
instead of freezing Emacs."
  (interactive)
  (if my/notes--syncing
      (message "notes: sync already in progress")
    (unless (my/notes--repo-p)
      (user-error "notes: %s is not a git repository" (my/notes--dir)))
    (setq my/notes--syncing t)
    (my/notes--run-git-async
     (list "pull" "--rebase" "--autostash")
     (lambda (status output)
       (if (/= status 0)
           (progn
             (setq my/notes--syncing nil)
             (message "notes: pull failed (%d):\n%s" status output))
         (if my/notes-push-after-pull
             (my/notes--run-git-async
              (list "push")
              (lambda (pstatus poutput)
                (setq my/notes--syncing nil)
                (message "notes: %s"
                         (if (= pstatus 0)
                             "pulled and pushed"
                           (format "pulled, but push failed (%d):\n%s"
                                   pstatus poutput)))))
           (setq my/notes--syncing nil)
           (message "notes: pulled")))))))

(when my/notes-pull-on-startup
  (add-hook 'emacs-startup-hook
            (lambda ()
              (run-with-idle-timer 5 nil #'my/notes-sync))))

;;; =========================================================================
;;; LATEX FRAGMENT -> TYPST FRAGMENT CONVERSION
;;; =========================================================================
;;; org-typst-preview uses `#(...)#' for inline and `#[ ... #]' for display
;;; math.  Old notes carry org LaTeX math ($...$, $$...$$, \[...\], and
;;; \begin{...} environments); this converts each fragment body with
;;; "pandoc -f latex -t typst" and only replaces it when the result compiles
;;; under "typst".  Anything that fails is left as LaTeX and reported, never
;;; silently mangled.

(defvar my/notes--skipped nil
  "LaTeX fragments that could not be converted, for reporting.")

(defun my/notes--latex-fragments ()
  "Return LaTeX fragments in the current buffer as (BEGIN END DISPLAY LATEX)."
  (require 'org-element)
  (let (frags)
    (org-element-map (org-element-parse-buffer) '(latex-fragment latex-environment)
      (lambda (el)
        (let* ((type (org-element-type el))
               (begin (org-element-property :begin el))
               (value (org-element-property :value el)))
          (push (list begin (+ begin (length value))
                      (or (eq type 'latex-environment)
                          (string-prefix-p "$$" value)
                          (string-prefix-p "\\[" value))
                      value)
                frags))))
    (nreverse frags)))

(defun my/notes--typst-body (latex)
  "Convert LaTeX fragment body LATEX to a Typst math body via pandoc.
Return the body string, or nil if pandoc is missing or fails."
  (with-temp-buffer
    (insert latex)
    (let ((out (get-buffer-create " *my-notes-pandoc*")))
      (with-current-buffer out (erase-buffer))
      (let ((exit (call-process-region (point-min) (point-max) "pandoc"
                                       nil out nil "-f" "latex" "-t" "typst")))
        (if (/= exit 0)
            nil
          (let ((text (string-trim (with-current-buffer out (buffer-string)))))
            (when (and (string-prefix-p "$" text)
                       (string-suffix-p "$" text)
                       (> (length text) 2))
              (string-trim (substring text 1 -1)))))))))

(defun my/notes--typst-compiles-p (body)
  "Return non-nil when BODY compiles as Typst math."
  (let ((dir (make-temp-file "my-notes-verify-" t)))
    (unwind-protect
        (let* ((in (expand-file-name "check.typ" dir))
               (svg (expand-file-name "check.svg" dir)))
          (with-temp-file in
            (insert "#set page(width: auto, height: auto, margin: 0pt)\n$"
                    body "$\n"))
          (= 0 (call-process "typst" nil nil nil
                             "compile" "--format" "svg" in svg)))
      (delete-directory dir t))))

(defun my/notes--typst-spans ()
  "Return ((BEGIN . END) ...) for org-typst-preview fragments in the buffer.

Stops us re-converting the escaped parens pandoc leaves inside a converted
body: after the first pass those sit between `#(...)'/`#[...#]', where org
would otherwise re-read them as fresh LaTeX fragments."
  (let (spans)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (rx "#" (or "(" "[")) nil t)
        (let* ((open (match-beginning 0))
               (closer (if (eq (char-before) ?\() "#)" "#]"))
               (escaped (and (> open (point-min))
                             (eq (char-before open) ?\\))))
          (when (and (not escaped)
                     (search-forward closer (point-max) t)
                     (not (eq (char-before (- (point) 2)) ?\\)))
            (push (cons open (point)) spans)))))
    spans))

(defun my/notes--fragment-replacement (display body)
  "Build the org-typst-preview fragment text for BODY (DISPLAY or inline)."
  (if display
      (concat "#[\n" body "\n#]")
    (concat "#(" (replace-regexp-in-string "[ \t]*\n[ \t]*" " " body) "#)")))

(defun my/notes--convert-file (file)
  "Convert LaTeX fragments in org FILE to Typst fragments, in place.
Return the number converted; unconvertible ones are left as LaTeX and
recorded in `my/notes--skipped'."
  (let ((changed 0))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((edits '())
            (inside-typst (my/notes--typst-spans)))
        (dolist (fr (my/notes--latex-fragments))
          (let* ((begin (nth 0 fr))
                 (body (my/notes--typst-body (nth 3 fr))))
            (cond
             ;; Already inside a converted org-typst-preview fragment: the
             ;; escaped parens pandoc leaves are not real LaTeX.
             ((seq-some (lambda (sp) (<= (car sp) begin (cdr sp)))
                        inside-typst)
              nil)
             ((and body
                   (my/notes--typst-compiles-p body)
                   (not (string-match-p "#)\\|#]" body)))
              (push (list begin (nth 1 fr)
                          (my/notes--fragment-replacement (nth 2 fr) body))
                    edits))
             (t
              (push (nth 3 fr) my/notes--skipped)))))
        (when edits
          (dolist (ed (sort edits (lambda (a b) (> (car a) (car b)))))
            (goto-char (car ed))
            (delete-region (car ed) (nth 1 ed))
            (insert (nth 2 ed)))
          (write-region nil nil file nil 'silent)
          (setq changed (length edits)))))
    changed))

(defun my/notes-latex-to-typst (&optional directory)
  "Convert LaTeX math in org notes under DIRECTORY to Typst fragments.

Each fragment is translated with pandoc (latex -> typst) and must compile
under typst before it is replaced; failures stay LaTeX and land in the
\"*notes-skipped*\" buffer.  Requires the `pandoc' and `typst' executables.
Returns the list of modified files without committing them."
  (interactive)
  (unless (executable-find "pandoc")
    (user-error "pandoc not found; install it (e.g. pacman -S pandoc)"))
  (unless (executable-find "typst")
    (user-error "typst not found"))
  (let* ((dir (file-name-as-directory (or directory (my/notes--dir))))
         (files (seq-remove (lambda (f) (string-match-p "/\\.git/" f))
                            (directory-files-recursively dir "\\.org\\'")))
         (changed '())
         (total 0))
    (setq my/notes--skipped nil)
    (dolist (file files)
      (when (file-readable-p file)
        (let ((n (my/notes--convert-file file)))
          (when (> n 0)
            (push file changed)
            (cl-incf total n)
            (message "notes: converted %d fragment(s) in %s"
                     n (file-name-nondirectory file))))))
    (let ((skipped (seq-uniq my/notes--skipped)))
      (message "notes: converted %d fragment(s) in %d file(s); %d skipped"
               total (length changed) (length skipped))
      (when skipped
        (with-current-buffer (get-buffer-create "*notes-skipped*")
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert "LaTeX fragments left as-is (conversion or compile failed):\n\n")
            (dolist (s skipped) (insert s "\n")))
          (display-buffer "*notes-skipped*")))
      (nreverse changed))))

(defun my/notes-convert-commit-push (&optional directory)
  "Convert LaTeX to Typst in notes, committing and pushing any changes."
  (interactive)
  (let ((changed (my/notes-latex-to-typst directory)))
    (if (null changed)
        (message "notes: nothing to convert")
      (my/notes--git "add" "-A")
      (let ((res (my/notes--git "commit" "-m"
                                "notes: convert LaTeX fragments to Typst")))
        (if (= (car res) 0)
            (progn (message "notes: committed %d file(s); pushing..."
                            (length changed))
                   (my/notes-sync))
          (message "notes: commit failed:\n%s" (cdr res)))))))

;;; =========================================================================
;;; KEYBINDINGS — C-c d p (sync), C-c d c (convert+commit+push)
;;; =========================================================================

(when (and (boundp 'my/denote-map) (keymapp my/denote-map))
  (keymap-set my/denote-map "p" #'my/notes-sync)
  (keymap-set my/denote-map "c" #'my/notes-convert-commit-push))

(provide 'config-notes)
;;; config-notes.el ends here
