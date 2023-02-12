;;; ../.dotfiles/.doom.d/biblio-zbmath.el -*- lexical-binding: t; -*-
(require 'biblio-core)
(require 'json)

;;;###autoload

(defun biblio-zbmath--url (query)
  (format "https://zbmath.org/citationmatching/match?t=%s&n=10" (url-encode-url query)))

(defun biblio-zbmath--bibtex-url (id)
  (format "https://zbmath.org/bibtex/%s.bib" id))

(defun biblio-zbmath-url-retrieve (url callback)
  "Wrapper around `url-queue-retrieve'.
 URL and CALLBACK; see `url-queue-retrieve'"
  (message "Fetching %s" url)
  (if biblio-synchronous
      (with-current-buffer (url-retrieve-synchronously url)
        (funcall callback nil))
    (setq url-queue-timeout 5)
    (url-queue-retrieve url callback)))

(defun biblio-zbmath--forward-bibtex (metadata forward-to)
  (let* ((id (biblio-alist-get 'id metadata))
         (url (biblio-zbmath--bibtex-url id)))
    (biblio-zbmath-url-retrieve url (biblio-generic-url-callback (lambda ()
                                                            (funcall forward-to (biblio-response-as-utf-8)))))))

(defun biblio-zbmath--format-result (result)
  (list
   (cons 'year (gethash "year" result))
   (cons 'authors (s-split "; " (gethash "authors" result)))
   (cons 'container (gethash "source" result))
   (cons 'references nil)
   (cons 'title (gethash "title" result))
   (cons 'id (gethash "zbl_id" result))))

(defun biblio-zbmath--parse-search-results ()
  (biblio-decode-url-buffer 'utf-8)
  (let ((results (json-parse-buffer)))
    (seq-map #'biblio-zbmath--format-result (gethash "results" results))))

(defun biblio-zbmath-backend (command &optional arg &rest more)
  (pcase command
    (`name  "ZBMath")
    (`prompt "ZBMath query: ")
    (`url (biblio-zbmath--url arg))
    (`forward-bibtex (biblio-zbmath--forward-bibtex arg (car more)))
    (`parse-buffer (biblio-zbmath--parse-search-results))
    (`register (add-to-list 'biblio-backends #'biblio-zbmath-backend))))

;;;###autoload
(add-hook 'biblio-init-hook #'biblio-zbmath-backend)

;;;###autoload
(defun biblio-zbmath-lookup (&optional query)
  "Start a ZBMath search for QUERY, prompting if needed."
  (interactive)
  (biblio-lookup #'biblio-zbmath-backend query))

;;;###autoload
(defalias 'zbmath-lookup 'biblio-zbmath-lookup)
