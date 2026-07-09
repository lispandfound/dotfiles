;;; config-windows.el --- Window management and popup system -*- lexical-binding: t; -*-

;;; =========================================================================
;;; DISPLAY-BUFFER-ALIST — route buffers to consistent positions
;;; =========================================================================

;; These rules tell Emacs WHERE to display specific buffers.
;; Popper (below) adds the toggle/dismiss lifecycle on top.
(dolist (rule
         `(;; Help and documentation — tall popup for comfortable reading
           (,(rx bos "*" (or "Help" "helpful " "Apropos" "info" "Info") (* any))
            (display-buffer-reuse-window display-buffer-at-bottom)
            (window-height . 0.40))

           ;; Build / search / diagnostic output — thin popup at bottom
           (,(rx bos "*" (or "compilation" "Compile-Log"
                             "grep" "xref" "occur" "Occur"
                             "Warnings" "Backtrace" "Messages"
                             "Async Shell Command"
                             "Flymake diagnostics" "Process List"))
            (display-buffer-reuse-window display-buffer-at-bottom)
            (window-height . 0.25))

           ;; Eat, vterm, shell, and the Python REPL — 30% bottom popup.
           ;; Plain eshell is deliberately excluded here: only popup eshells
           ;; (see `my/popup-eshell-p' below) get this treatment, so that a
           ;; regular `M-x eshell' opens like any other buffer.
           (,(rx bos "*" (or "eat" "vterm" "shell" "terminal" "Python") (* any))
            (display-buffer-reuse-window display-buffer-at-bottom)
            (window-height . 0.30))

           ;; Popup eshells — matched by the `my/popup-eshell-p' marker, not
           ;; by name, so this only ever catches buffers `my/popup-eshell'
           ;; created.
           (my/popup-eshell-buffer-p
            (display-buffer-reuse-window display-buffer-at-bottom)
            (window-height . 0.30))))
  (add-to-list 'display-buffer-alist rule))

;;; =========================================================================
;;; POPPER — popup buffer toggle/cycle/dismiss
;;; =========================================================================

(use-package popper
  ;; after-init triggers the load and activates popper-mode.
  ;; :bind alone would defer the package and popper-mode would never run.
  :hook (after-init . popper-mode)
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :custom
  (popper-window-height 0.30)
  (popper-reference-buffers
   '("\\*Messages\\*"
     "\\*Warnings\\*"
     "\\*Compile-Log\\*"
     "\\*Backtrace\\*"
     "Output\\*$"
     "\\*Async Shell Command\\*"
     "\\*Flymake diagnostics.*\\*"
     "\\*xref\\*"
     "\\*grep\\*"
     "\\*occur\\*"
     "\\*Process List\\*"
     help-mode
     helpful-mode
     compilation-mode
     my/popup-eshell-buffer-p
     eat-mode
     vterm-mode
     inferior-python-mode))
  :config
  (popper-mode 1)
  (popper-echo-mode 1))

;;; =========================================================================
;;; POPUP ESHELL — Doom-style toggleable eshell at the bottom
;;; =========================================================================

(defvar-local my/popup-eshell-p nil
  "Non-nil in eshell buffers created by `my/popup-eshell'.
This is what lets those buffers be routed to a bottom popup (see
`display-buffer-alist' and `popper-reference-buffers') while a plain
`M-x eshell' buffer is left to display and behave normally.")

(defun my/popup-eshell-buffer-p (buffer-or-name &optional _action)
  "Return non-nil if BUFFER-OR-NAME is a popup eshell.
Takes an optional second ACTION argument (ignored) so it can double as
a `display-buffer-alist' condition as well as a `popper-reference-buffers'
predicate."
  (when-let ((buf (get-buffer buffer-or-name)))
    (buffer-local-value 'my/popup-eshell-p buf)))

;; Forward declaration so the `let'-binding below binds the real dynamic
;; variable from esh-mode.el, not a throwaway lexical one — eshell isn't
;; necessarily loaded yet when this file is compiled.
(defvar eshell-buffer-name)

(defun my/popup-eshell ()
  "Toggle a popup eshell at the bottom of the frame.

Only operates on eshells marked via `my/popup-eshell-p' — i.e. ones this
command itself created — so a regular `M-x eshell' buffer is never
hijacked as the popup.

If a popup eshell window is already visible:
  - and it is the selected window → dismiss it
  - otherwise → jump to it
If no popup eshell window is visible, open or create one at the bottom."
  (interactive)
  (let ((win (cl-find-if
              (lambda (w) (my/popup-eshell-buffer-p (window-buffer w)))
              (window-list nil 'no-minibuffer))))
    (cond
     ;; Popup eshell focused → close popup
     ((eq win (selected-window))
      (delete-window win))
     ;; Popup eshell visible elsewhere → focus it
     (win
      (select-window win))
     ;; Not visible → open existing popup-eshell buffer or create a new one
     (t
      (let* ((buf (or (cl-find-if #'my/popup-eshell-buffer-p (buffer-list))
                      ;; None exists yet — create one without disturbing windows.
                      (let ((eshell-buffer-name "*popup-eshell*"))
                        (save-window-excursion (eshell) (current-buffer))))))
        (with-current-buffer buf
          (setq my/popup-eshell-p t))
        (select-window
         (display-buffer buf '((display-buffer-at-bottom)
                               (window-height . 0.30)))))))))

;;; =========================================================================
;;; OTHER-WINDOW-MRU — jump back to the most-recently-used window
;;; =========================================================================
;;; Described in "The Emacs Window Management Almanac" as the cleanest way
;;; to bounce between two windows without caring about their spatial order.

(defun my/other-window-mru ()
  "Select the most recently used window on this frame.
Unlike `other-window', this uses history rather than spatial order —
ideal for bouncing between two windows."
  (interactive)
  (when-let ((mru (get-mru-window nil nil 'not-this-one-dummy)))
    (select-window mru)))

;;; =========================================================================
;;; ACE-WINDOW — visual window selection and routing
;;; =========================================================================

(use-package ace-window
  :custom
  (aw-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)) ; match avy key set
  (aw-scope 'frame)
  (aw-background t)
  :bind ("M-O" . ace-window)) ; M-o → mru (below), M-O → ace pick

;;; Dispatch: run a single command in any ace-selected window.
;;; Adapted from "The Emacs Window Management Almanac".
(defun my/ace-window-one-command ()
  "Select a window with ace-window and run one command in it.
Useful for looking up something in a reference window without leaving."
  (interactive)
  (when-let ((win (aw-select " ACE")))
    (when (windowp win)
      (with-selected-window win
        (let* ((cmd (key-binding
                     (read-key-sequence
                      (format "Run in [%s]: " (buffer-name)))))
               (this-command cmd))
          (call-interactively cmd))))))

;;; Prefix: display the NEXT command's buffer in an ace-selected window.
;;; Analogous to `C-x 4 …' but without committing to a direction up front.
(defun my/ace-window-prefix ()
  "Use ace-window to display the buffer of the next command.
The buffer opened by the immediately following command will appear in
the ace-selected window instead of the default location."
  (interactive)
  (display-buffer-override-next-command
   (lambda (buffer _)
     (cons (aw-select (propertize " ACE" 'face 'mode-line-highlight))
           'reuse))
   nil "[ace-window]")
  (message "ace-window: next buffer will open in selected window…"))

;;; =========================================================================
;;; TAB-BAR / WINDOW TRANSIENT — C-c w
;;; Replaces the old my/window-map prefix keymap: same keys, but winner
;;; undo/redo and tab next/prev repeat without re-pressing the prefix, and
;;; open tabs are listed as selectable menu entries.
;;; =========================================================================

(require 'transient)

(defun my/tab-bar--select-suffixes (_)
  "Build one transient suffix per open tab (up to 9), keyed by tab number."
  (transient-parse-suffixes
   'my/tab-bar-tmenu
   (seq-map-indexed
    (lambda (tab i)
      (let ((n (1+ i))
            (name (alist-get 'name tab)))
        (list (number-to-string n)
              (if (eq (car tab) 'current-tab)
                  (format "%s (current)" name)
                name)
              (lambda () (interactive) (tab-bar-select-tab n)))))
    (seq-take (funcall tab-bar-tabs-function) 9))))

(transient-define-prefix my/tab-bar-tmenu ()
  "Window and tab-bar workspace commands."
  ["Windows"
   [("-" "Split below" split-window-below)
    ("|" "Split right" split-window-right)
    ("t" "Popup eshell" my/popup-eshell)]
   [("d" "Delete" delete-window)
    ("o" "Delete others" delete-other-windows)]
   [("u" "Winner undo" winner-undo :transient t)
    ("U" "Winner redo" winner-redo :transient t)]]
  ["Tabs"
   [("N" "New" tab-new)
    ("c" "Close" tab-close)
    ("r" "Rename" tab-rename)]
   [("n" "Next" tab-next :transient t)
    ("p" "Previous" tab-previous :transient t)
    ("w" "Switch…" tab-switch)]]
  ["Select tab"
   :class transient-row
   :setup-children my/tab-bar--select-suffixes])

(keymap-global-set "C-c w" #'my/tab-bar-tmenu)

(provide 'config-windows)
;;; config-windows.el ends here
