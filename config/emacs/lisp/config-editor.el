;;; config-editor.el --- Editor enhancements -*- lexical-binding: t; -*-

;;; =========================================================================
;;; APHELEIA — async code formatting on save
;;; =========================================================================

(use-package apheleia
  :config
  (apheleia-global-mode 1))

;;; =========================================================================
;;; YASNIPPET — snippet expansion
;;; =========================================================================

(use-package yasnippet
  :custom
  (yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory)))
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets)

;;; =========================================================================
;;; AUTO-INSERT — file templates (built-in)
;;; =========================================================================

(use-package autoinsert
  :ensure nil
  :custom (auto-insert-query nil)
  :config (auto-insert-mode 1))

;;; =========================================================================
;;; HIDESHOW — code folding (built-in)
;;; =========================================================================

(use-package hideshow
  :ensure nil
  :hook (prog-mode . hs-minor-mode)
  :bind (:map hs-minor-mode-map
              ;; C-c h is the prefix; h/H/S/l hang off it.
              ("C-c h h" . hs-toggle-hiding)
              ("C-c h H" . hs-hide-all)
              ("C-c h S" . hs-show-all)
              ("C-c h l" . hs-hide-level)))

;;; =========================================================================
;;; EXPAND-REGION — grow/shrink selection by semantic units
;;; Bound globally to C-= in config-keys.el.
;;; =========================================================================

(use-package expand-region
  :commands (er/expand-region er/contract-region er/mark-symbol er/mark-word))

;;; =========================================================================
;;; DRAG-STUFF — move lines or regions up/down with M-up/M-down
;;; =========================================================================

(use-package drag-stuff
  :config
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))   ; sets up M-up / M-down / M-left / M-right

;;; =========================================================================
;;; LINK-HINT — avy-style link jumping in text / help / info buffers
;;; =========================================================================

(use-package link-hint
  :bind ("C-c o l" . link-hint-open-link)
  :config
  (with-eval-after-load 'help-mode
    (keymap-set help-mode-map "o" #'link-hint-open-link))
  (with-eval-after-load 'info
    (keymap-set Info-mode-map "o" #'link-hint-open-link))
  (with-eval-after-load 'apropos
    (keymap-set apropos-mode-map "o" #'link-hint-open-link)))

;;; =========================================================================
;;; VUNDO — visual undo tree
;;; =========================================================================

(use-package vundo
  :bind ("C-x u" . vundo)
  :custom (vundo-glyph-alist vundo-unicode-symbols))

;; Increase undo limits (Doom's :emacs undo module does this).
(use-package emacs
  :ensure nil
  :custom
  (undo-limit        (* 80  1024 1024))
  (undo-strong-limit (* 120 1024 1024))
  (undo-outer-limit  (* 360 1024 1024)))

;;; =========================================================================
;;; CASUAL SUITE — transient menus for built-in modes
;;; Full replication of editor/casual custom module.
;;;
;;; `casual' is the monorepo package; all casual-* sub-packages are provided
;;; by it and must use :ensure nil.  casual-avy is a separate package.
;;; =========================================================================

(use-package casual :ensure t)

(use-package casual-calc
  :ensure nil
  :after calc
  :bind (:map calc-mode-map ("C-o" . casual-calc-tmenu)))

(use-package casual-info
  :ensure nil
  :after info
  :bind (:map Info-mode-map ("C-o" . casual-info-tmenu)))

(use-package casual-dired
  :ensure nil
  :after (dired transient)
  :bind (:map dired-mode-map ("C-o" . casual-dired-tmenu)))

(use-package casual-avy
  :ensure t
  :bind ("M-g" . my/custom-avy-tmenu)
  :config
  (defun my/custom-avy-tmenu ()
    "Custom avy transient menu extended with consult integrations."
    (interactive)
    (require 'casual-avy)
    (transient-append-suffix 'casual-avy-tmenu "M-n"
      '("E" "Error" consult-compile-error :transient nil))
    (transient-append-suffix 'casual-avy-tmenu "E"
      '("f" "Flymake Error" consult-flymake))
    (transient-append-suffix 'casual-avy-tmenu "p"
      '("o" "Outline Item" consult-outline))
    (transient-append-suffix 'casual-avy-tmenu "o"
      '("i" "Imenu Item" consult-imenu))
    (transient-append-suffix 'casual-avy-tmenu "i"
      '("j" "Resume last jump" avy-resume))
    (casual-avy-tmenu)))

(use-package casual-make
  :ensure nil
  :after make-mode
  :bind (:map makefile-mode-map ("C-o" . casual-make-tmenu)))

(use-package casual-isearch
  :ensure nil
  :bind (:map isearch-mode-map ("C-o" . casual-isearch-tmenu)))

(use-package casual-ibuffer
  :ensure nil
  :after ibuffer
  :bind (:map ibuffer-mode-map
              ("C-o" . casual-ibuffer-tmenu)
              ("F"   . casual-ibuffer-filter-tmenu)
              ("s"   . casual-ibuffer-sortby-tmenu)
              ("{"   . ibuffer-backwards-next-marked)
              ("}"   . ibuffer-forward-next-marked)
              ("["   . ibuffer-backward-filter-group)
              ("]"   . ibuffer-forward-filter-group)
              ("$"   . ibuffer-toggle-filter-group))
  :config
  (transient-define-prefix casual-ibuffer-groupby-tmenu ()
    "Casual IBuffer submenu for switching between grouping strategies."
    ["IBuffer: Group By"
     [("p" "Project" my/ibuffer-groupby-project)
      ("d" "Directory Suffix" my/ibuffer-groupby-directory-suffix)]
     [("r" "Remote Host" my/ibuffer-groupby-remote)
      ("m" "Major Mode" ibuffer-set-filter-groups-by-mode)]
     [("n" "None" ibuffer-clear-filter-groups)]]

    [:class transient-row
            (casual-lib-quit-one)
            (casual-lib-quit-all)])

  (transient-append-suffix 'casual-ibuffer-tmenu "$"
    '("G" "Group By›" casual-ibuffer-groupby-tmenu)))

(use-package casual-image
  :ensure nil
  :bind (:map image-mode-map ("C-o" . casual-image-tmenu)))

(use-package casual-bookmarks
  :ensure nil
  :after bookmark
  :bind (:map bookmark-bmenu-mode-map
              ("C-o" . casual-bookmarks-tmenu)
              ("S"   . casual-bookmarks-sortby-tmenu)
              ("J"   . bookmark-jump)))

(use-package casual-agenda
  :ensure nil
  :after org-agenda
  :bind (:map org-agenda-mode-map
              ("C-o" . casual-agenda-tmenu)
              ("M-j" . org-agenda-clock-goto)
              ("J"   . bookmark-jump)))

(use-package casual-editkit
  :ensure nil
  :bind ("C-c C-h" . casual-editkit-main-tmenu)
  :hook (rectangle-mark-mode
         . (lambda ()
             (keymap-set rectangle-mark-mode-map "C-o"
                         #'casual-editkit-rectangle-tmenu))))

(use-package casual-compile
  :ensure nil
  :after compile
  :bind (:map compilation-mode-map ("C-o" . casual-compile-tmenu))
  :config
  ;; grep-mode derives from compilation but its keymap does not inherit
  ;; compilation-mode-map, so bind there too.
  (with-eval-after-load 'grep
    (keymap-set grep-mode-map "C-o" #'casual-compile-tmenu)))

(use-package casual-eshell
  :ensure nil
  :after eshell
  :bind (:map eshell-mode-map ("C-o" . casual-eshell-tmenu)))

(use-package casual-eww
  :ensure nil
  :after eww
  :bind (:map eww-mode-map ("C-o" . casual-eww-tmenu)))

(use-package casual-help
  :ensure nil
  :after help-mode
  :bind (:map help-mode-map ("C-o" . casual-help-tmenu)))

(use-package casual-man
  :ensure nil
  :after man
  :bind (:map Man-mode-map ("C-o" . casual-man-tmenu)))

(use-package casual-re-builder
  :ensure nil
  :after re-builder
  :bind (:map reb-mode-map      ("C-o" . casual-re-builder-tmenu)
         :map reb-lisp-mode-map ("C-o" . casual-re-builder-tmenu)))

(use-package casual-ediff
  :ensure nil
  :after ediff
  :config
  ;; ediff builds its control-buffer keymap dynamically per session.
  (add-hook 'ediff-keymap-setup-hook
            (lambda () (keymap-set ediff-mode-map "C-o" #'casual-ediff-tmenu))))

;; Editing modes keep C-o = open-line; major-mode menus live on C-c m.
(use-package casual-elisp
  :ensure nil
  :bind (:map emacs-lisp-mode-map ("C-c m" . casual-elisp-tmenu)))

(use-package casual-org
  :ensure nil
  :after org
  :bind (:map org-mode-map ("C-c m" . casual-org-tmenu)))

;;; =========================================================================
;;; IBUFFER GROUPING — switch how `ibuffer' groups buffers on the fly
;;; via casual-ibuffer's "Group By" menu (see `casual-ibuffer-groupby-tmenu'
;;; above): by project, by directory, by remote host, or by major mode.
;;; =========================================================================
(use-package ibuffer-project
  :hook (ibuffer-hook . (lambda ()
                          (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups)))))

(defun my/ibuffer-groupby-project ()
  "Group ibuffer buffers by project, using `ibuffer-project'."
  (interactive)
  (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
  (ibuffer-update nil t))

(defun my/ibuffer--buffer-directory (buf)
  "Return the directory BUF would be grouped under, or nil."
  (with-current-buffer buf
    (if buffer-file-name
        (file-name-directory buffer-file-name)
      default-directory)))

(defun my/ibuffer--unique-dir-suffix (dir all-dirs)
  "Return the shortest trailing path of DIR that stays unique among ALL-DIRS.
Mirrors the disambiguation `uniquify-buffer-name-style' does for buffer
names, but for directories: start from just the leaf directory name and
grow the suffix leftward only as far as needed to stay distinct from
every other directory in ALL-DIRS."
  (let* ((components (nreverse (split-string (directory-file-name dir) "/" t)))
         (others (remove dir all-dirs))
         (max-n (length components))
         (n 1))
    (while (and (< n max-n)
                (seq-some (lambda (other)
                            (equal (seq-take components n)
                                   (seq-take (nreverse (split-string (directory-file-name other) "/" t)) n)))
                          others))
      (setq n (1+ n)))
    (concat (unless (= n max-n) "…/")
            (mapconcat #'identity (reverse (seq-take components n)) "/"))))

(defun my/ibuffer-groupby-directory-suffix ()
  "Group ibuffer buffers by directory, naming groups by shortest unique suffix."
  (interactive)
  (let ((dirs (seq-uniq (delq nil (mapcar #'my/ibuffer--buffer-directory (buffer-list))))))
    (setq ibuffer-filter-groups
          (mapcar (lambda (dir)
                    (cons (my/ibuffer--unique-dir-suffix dir dirs)
                          (list (cons 'directory (concat "\\`" (regexp-quote dir) "\\'")))))
                  dirs)))
  (ibuffer-update nil t))

(defun my/ibuffer--buffer-remote-host (buf)
  "Return \"METHOD:HOST\" for BUF's TRAMP connection, or nil if BUF is local."
  (with-current-buffer buf
    (let ((file (or buffer-file-name default-directory)))
      (when (file-remote-p file)
        (format "%s:%s" (file-remote-p file 'method) (file-remote-p file 'host))))))

(defun my/ibuffer-groupby-remote ()
  "Group ibuffer buffers by TRAMP remote host, with a separate \"Local\" group."
  (interactive)
  (let ((hosts (seq-uniq (delq nil (mapcar #'my/ibuffer--buffer-remote-host (buffer-list))))))
    (setq ibuffer-filter-groups
          (append
           (mapcar (lambda (host)
                     `(,host (predicate equal (my/ibuffer--buffer-remote-host (current-buffer)) ,host)))
                   hosts)
           '(("Local" (predicate not (my/ibuffer--buffer-remote-host (current-buffer))))))))
  (ibuffer-update nil t))

(provide 'config-editor)
;;; config-editor.el ends here
