;;; casual/config.el -*- lexical-binding: t; -*-


;; (use-package! casual-suite
;;   :defer t)

(use-package! casual-calc  
   :bind (:map calc-mode-map ("C-o" . casual-calc-tmenu))
   :after (calc))

(use-package! casual-info
  :bind (:map Info-mode-map ("C-o" . casual-info-tmenu))
  :after (info))

(use-package! casual-dired
  :bind (:map dired-mode-map ("C-o" . casual-dired-tmenu))
  :after (dired transient))

(use-package! casual-avy
  :bind ("M-g" . my/custom-avy-tmenu)
  :init
  (defun my/custom-avy-tmenu ()
    (interactive)
    (require 'casual-avy)
    (transient-append-suffix 'casual-avy-tmenu "M-n"  '("E" "Error" consult-compile-error :transient nil))
    (transient-append-suffix 'casual-avy-tmenu "E"  '("f" "Flymake Error" consult-flycheck))
    (transient-append-suffix 'casual-avy-tmenu "p"  '("o" "Outline Item" consult-outline))
    (transient-append-suffix 'casual-avy-tmenu "o"  '("i" "Imenu Item" consult-imenu))
    (transient-append-suffix 'casual-avy-tmenu "i"  '("j" "Resume last jump" avy-resume))
    (casual-avy-tmenu)))

(use-package! casual-make
  :bind (:map makefile-mode-map ("C-o" . casual-make-tmenu))
  :after (make-mode))

(use-package! casual-isearch
  :bind (:map isearch-mode-map ("C-o" . casual-isearch-tmenu)))


(use-package! casual-ibuffer
  :bind (:map
         ibuffer-mode-map
         ("C-o" . casual-ibuffer-tmenu)
         ("F" . casual-ibuffer-filter-tmenu)
         ("s" . casual-ibuffer-sortby-tmenu)
         ("<double-mouse-1>" . ibuffer-visit-buffer) ; optional
         ("M-<double-mouse-1>" . ibuffer-visit-buffer-other-window) ; optional
         ("{" . ibuffer-backwards-next-marked) ; optional
         ("}" . ibuffer-forward-next-marked)   ; optional
         ("[" . ibuffer-backward-filter-group) ; optional
         ("]" . ibuffer-forward-filter-group)  ; optional
         ("$" . ibuffer-toggle-filter-group))  ; optional
  :after (ibuffer))


(use-package! casual-bookmarks
  :ensure nil
  :bind (:map bookmark-bmenu-mode-map
              ("C-o" . casual-bookmarks-tmenu)
              ("S" . casual-bookmarks-sortby-tmenu)
              ("J" . bookmark-jump))
  :after (bookmark))

(use-package! casual-agenda
  :after org-agenda  ;; Ensure org-agenda is loaded first
  :bind (:map
         org-agenda-mode-map
         ("C-o" . casual-agenda-tmenu)
         ("M-j" . org-agenda-clock-goto) ; optional
         ("J" . bookmark-jump))) ; optional

(use-package! casual-editkit
  :bind (("C-c C-h" . casual-editkit-main-tmenu))
  :hook (rectangle-mark-mode . (lambda ()
                                 (define-key rectangle-mark-mode-map (kbd "C-o") #'casual-editkit-rectangle-tmenu))))
