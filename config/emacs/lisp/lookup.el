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

(defvar lookup/sources
  '(("DuckDuckGo" . "https://duckduckgo.com/?q=%s")
    ("GitHub" . "https://github.com/search?type=code&q=%s")
    ("ucgmsim org" . "https://github.com/search?type=code&q=org%%3Aucgmsim%%20%s"))
  "Alist of search providers and their URL templates.")

(defvar lookup/query-history nil
  "History of queries used in `lookup/query`.")

(defvar lookup/history-file
  (expand-file-name "my-lookup-history.el" user-emacs-directory)
  "File where `lookup/query-history` is saved.")

;;;###autoload
(defun lookup/save-query-history ()
  "Save `lookup/query-history` to disk."
  (with-temp-file lookup/history-file
    (insert (format ";; Auto-generated lookup query history\n(setq lookup/query-history '%S)\n"
                    lookup/query-history))))

;;;###autoload
(defun lookup/load-query-history ()
  "Load `lookup/query-history` from disk, if it exists."
  (when (file-exists-p lookup/history-file)
    (load-file lookup/history-file)))

;;;###autoload
(defun lookup/query (query)
  "Prompt for a search provider and search for QUERY in browser."
  (interactive
   (list (read-string "Search for: " nil 'lookup/query-history (thing-at-point 'symbol t))))
  (unless lookup/query-history
    ;; Initialize history on load
    (lookup/load-query-history))
  (let* ((provider (completing-read "Search with: " (mapcar #'car lookup/sources)))
         (template (cdr (assoc provider lookup/sources))))
    (add-to-list 'lookup/query-history query)
    (browse-url (format template (url-hexify-string query)))))

;; Save on exit
(add-hook 'kill-emacs-hook #'lookup/save-query-history)


(provide 'lookup)
;;; lookup.el ends here
