;;; lookup.el --- Lookup stuff online -*- lexical-binding: t; -*-
;;
;; Author: Your Name <your@email.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, tools, web
;; URL: https://github.com/yourname/lookup.el
;;
;;; Commentary:
;;
;; Simple interface to search online via DuckDuckGo, GitHub, etc.
;; Includes query history saving and Embark integration.
;;
;;; Code:

(defvar jake/lookup-sources
  '(("DuckDuckGo" . "https://duckduckgo.com/?q=%s")
    ("GitHub" . "https://github.com/search?type=code&q=%s")
    ("ucgmsim org" . "https://github.com/search?type=code&q=org%%3Aucgmsim%%20%s"))
  "Alist of search providers and their URL templates.")

(defvar jake/lookup-query-history nil
  "History of queries used in `jake/lookup-query`.")

(defvar jake/lookup-history-file
  (expand-file-name "my-lookup-history.el" user-emacs-directory)
  "File where `jake/lookup-query-history` is saved.")

;;;###autoload
(defun jake/lookup-save-query-history ()
  "Save `jake/lookup-query-history` to disk."
  (with-temp-file jake/lookup-history-file
    (insert (format ";; Auto-generated lookup query history\n(setq jake/lookup-query-history '%S)\n"
                    jake/lookup-query-history))))

;;;###autoload
(defun jake/lookup-load-query-history ()
  "Load `jake/lookup-query-history` from disk, if it exists."
  (when (file-exists-p jake/lookup-history-file)
    (load-file jake/lookup-history-file)))

;;;###autoload
(defun jake/lookup-query (query)
  "Prompt for a search provider and search for QUERY in browser."
  (interactive
   (list (read-string "Search for: " nil 'jake/lookup-query-history (thing-at-point 'symbol t))))
  (unless jake/lookup-query-history
    ;; Initialize history on load
    (jake/lookup-load-query-history))
  (let* ((provider (completing-read "Search with: " (mapcar #'car jake/lookup-sources)))
         (template (cdr (assoc provider jake/lookup-sources))))
    (add-to-list 'jake/lookup-query-history query)
    (browse-url (format template (url-hexify-string query)))))

;; Save on exit
(add-hook 'kill-emacs-hook #'jake/lookup-save-query-history)


(provide 'lookup)
;;; lookup.el ends here
