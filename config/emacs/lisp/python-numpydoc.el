;;; python-numpydoc.el --- Generate NumPy-style docstrings for Python functions -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Your Name <your.email@example.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1") (s "1.12.0") (dash "2.19.0"))
;; Keywords: python, documentation, docstring, numpy
;; URL: https://github.com/yourusername/python-numpydoc

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides functionality to automatically generate NumPy-style
;; docstrings for Python functions and methods using tree-sitter.
;;
;; Features:
;; - Automatic parameter extraction with type annotations
;; - Return type detection
;; - Exception detection from raise statements
;; - Customisation docstring templates
;; - Interactive prompts for descriptions
;; - Support for both functions and methods
;;
;; Usage:
;; Place your cursor inside a Python function and call `python-numpydoc-insert'.
;; The package will analyse the function signature and guide you through
;; creating a complete NumPy-style docstring.

;;; Code:

(require 's)
(require 'dash)

;;; Customization

(defgroup python-numpydoc nil
  "Generate NumPy-style docstrings for Python functions."
  :group 'python
  :prefix "python-numpydoc-")

(defcustom python-numpydoc-quote-style "\"\"\""
  "Style of quotes to use for docstrings."
  :type '(choice (const :tag "Triple double quotes" "\"\"\"")
                 (const :tag "Triple single quotes" "'''"))
  :group 'python-numpydoc)

(defcustom python-numpydoc-prompt-for-long-description t
  "Whether to prompt for a long description in addition to short description."
  :type 'boolean
  :group 'python-numpydoc)

(defcustom python-numpydoc-auto-fill-paragraphs t
  "Whether to automatically fill paragraph text in docstrings."
  :type 'boolean
  :group 'python-numpydoc)

(defcustom python-numpydoc-include-parameter-types t
  "Whether to include parameter types in the docstring."
  :type 'boolean
  :group 'python-numpydoc)

(defcustom python-numpydoc-skip-self-parameter t
  "Whether to skip 'self' and 'cls' parameters in method docstrings."
  :type 'boolean
  :group 'python-numpydoc)

(defcustom python-numpydoc-overwrite-existing-prompt t
  "Whether to prompt before overwriting existing docstrings."
  :type 'boolean
  :group 'python-numpydoc)

;;; Internal Functions

(defun python-numpydoc--get-function-definition ()
  "Get the tree-sitter node for the current function or class definition.
  Returns the nearest function_definition or class_definition node containing point."
  (treesit-parent-until
   (treesit-node-at (point))
   (lambda (node)
     (or (string= (treesit-node-type node) "function_definition")
         (string= (treesit-node-type node) "class_definition")))))

(defun python-numpydoc--extract-parameters (node)
  "Extract parameters from function definition NODE.
   Returns a list of cons cells (name . type) for each parameter."
  (when-let ((parameters (treesit-node-child-by-field-name node "parameters")))
    (-map (lambda (param-node)
            (cons (python-numpydoc--get-parameter-name param-node)
                  (python-numpydoc--get-parameter-type param-node)))
          (-filter #'python-numpydoc--get-parameter-name
                   (treesit-node-children parameters t)))))

(defun python-numpydoc--is-method-p (node)
  "Check if function definition NODE is a method inside a class.
   Returns t if the function is defined within a class_definition."
  (when-let ((grandparent (treesit-node-parent (treesit-node-parent node))))
    (string= (treesit-node-type grandparent) "class_definition")))

(defun python-numpydoc--get-parameter-name (node)
  "Extract parameter name from parameter NODE.
   Returns the parameter name as a string, or nil if not found."
  (when-let ((identifier (treesit-search-subtree node "identifier")))
    (substring-no-properties (treesit-node-text identifier))))

(defun python-numpydoc--extract-annotated-type (node)
  "Extract the inner type from Annotated type hints in NODE."
  (alist-get 't
             (treesit-query-capture
              node
              '(((type (generic_type
                        (identifier) @gener
                        (type_parameter (type (identifier) @t) _)))
                 (:match "Annotated" @gener))))))

(defun python-numpydoc--get-parameter-type (node)
  "Extract parameter type annotation from parameter NODE."
  (when-let ((type-node (treesit-node-child-by-field-name node "type")))
    (let* ((raw-type (treesit-node-text type-node))
           (annotated-type (python-numpydoc--extract-annotated-type type-node))
           (final-type (if annotated-type
                           (treesit-node-text annotated-type)
                         raw-type)))
      (when final-type
        (s-replace-regexp "[[:space:]\n]+" " "
                          (substring-no-properties final-type))))))

(defun python-numpydoc--get-return-type (node)
  "Extract return type annotation from function definition NODE."
  (when-let ((return-node (treesit-node-child-by-field-name node "return_type")))
    (s-replace-regexp "[[:space:]\n]+" " "
                      (substring-no-properties (treesit-node-text return-node)))))

(defun python-numpydoc--extract-docstring (node)
  "Extract existing docstring from function definition NODE.
  Returns the docstring node if found, nil otherwise."
  (alist-get 'c
             (treesit-query-capture
              node
              '((function_definition
                 body: (block :anchor (expression_statement (string) @c)))))))

(defun python-numpydoc--extract-exceptions (node)
  "Extract exception types from raise statements in function NODE.
  Returns a list of exception type names."
  (-map (lambda (match) (treesit-node-text (cdr match)))
        (treesit-query-capture node '((raise_statement
                                       (call function: (_) @e))))))

(defun python-numpydoc--filter-parameters (parameters is-method)
  "Filter PARAMETERS list based on method status and configuration.
  Removes 'self' and 'cls' parameters from methods if configured to do so."
  (if (and is-method python-numpydoc-skip-self-parameter)
      (-filter (lambda (param)
                 (let ((name (car param)))
                   (and (not (string= name "self"))
                        (not (string= name "cls")))))
               parameters)
    parameters))

(defun python-numpydoc--prompt-for-description (prompt-text)
  "Prompt user for description with PROMPT-TEXT.
  Returns the trimmed input string."
  (s-trim (read-from-minibuffer prompt-text)))

(defun python-numpydoc--insert-section-header (title underline-char)
  "Insert a NumPy docstring section header with TITLE and UNDERLINE-CHAR."
  (newline-and-indent 2)
  (insert title)
  (newline-and-indent)
  (insert (make-string (length title) underline-char))
  (newline-and-indent))

(defun python-numpydoc--insert-parameter-section (parameters)
  "Insert the Parameters section for PARAMETERS list."
  (python-numpydoc--insert-section-header "Parameters" ?-)
  (dolist (parameter parameters)
    (let ((name (car parameter))
          (type (cdr parameter)))
      (insert name)
      (when (and type python-numpydoc-include-parameter-types)
        (insert " : " type))
      (newline-and-indent)
      (let ((description (python-numpydoc--prompt-for-description
                          (format "Description for %s: " name))))
        (insert description)
        (call-interactively #'python-indent-shift-right)
        (when python-numpydoc-auto-fill-paragraphs
          (beginning-of-line)
          (set-mark-command nil)
          (end-of-line)
          (fill-region (region-beginning) (region-end))
          (deactivate-mark)))
      (newline-and-indent))))

(defun python-numpydoc--insert-returns-section (return-type)
  "Insert the Returns section for RETURN-TYPE."
  (python-numpydoc--insert-section-header "Returns" ?-)
  (insert return-type)
  (newline-and-indent)
  (let ((description (python-numpydoc--prompt-for-description
                      "Description of return value: ")))
    (insert description)
    (call-interactively #'python-indent-shift-right)
    (when python-numpydoc-auto-fill-paragraphs
      (beginning-of-line)
      (set-mark-command nil)
      (end-of-line)
      (fill-region (region-beginning) (region-end))
      (deactivate-mark))))

(defun python-numpydoc--insert-raises-section (exceptions)
  "Insert the Raises section for EXCEPTIONS list."
  (python-numpydoc--insert-section-header "Raises" ?-)
  (dolist (exception exceptions)
    (insert exception)
    (newline-and-indent)
    (let ((description (python-numpydoc--prompt-for-description
                        (format "Description for exception %s: " exception))))
      (insert description)
      (call-interactively #'python-indent-shift-right)
      (when python-numpydoc-auto-fill-paragraphs
        (beginning-of-line)
        (set-mark-command nil)
        (end-of-line)
        (fill-region (region-beginning) (region-end))
        (deactivate-mark)))
    (newline-and-indent)))

;;; Public Interface

;;;###autoload
(defun python-numpydoc-insert ()
  "Insert a NumPy-style docstring for the current Python function.
  Analyses the function signature using tree-sitter and prompts for
  descriptions of parameters, return values, and exceptions."
  (interactive)
  (unless (treesit-language-available-p 'python)
    (user-error "Tree-sitter for Python is not available"))

  (save-excursion
    (let* ((function-def (python-numpydoc--get-function-definition))
           (is-method (python-numpydoc--is-method-p function-def))
           (all-parameters (python-numpydoc--extract-parameters function-def))
           (parameters (python-numpydoc--filter-parameters all-parameters is-method))
           (return-type (python-numpydoc--get-return-type function-def))
           (exceptions (python-numpydoc--extract-exceptions function-def))
           (existing-docstring (python-numpydoc--extract-docstring function-def)))

      (unless function-def
        (user-error "Not inside a Python function or class definition"))

      ;; Handle existing docstring
      (when existing-docstring
        (if (and python-numpydoc-overwrite-existing-prompt
                 (not (yes-or-no-p "Overwrite existing docstring? ")))
            (user-error "Cancelled")
          (delete-region (treesit-node-start existing-docstring)
                         (treesit-node-end existing-docstring))))

      ;; Insert docstring template
      (goto-char (treesit-node-start
                  (treesit-node-child-by-field-name function-def "body")))
      (insert python-numpydoc-quote-style)
      (newline-and-indent)
      (insert python-numpydoc-quote-style)
      (newline-and-indent)
      (previous-line 2)
      (end-of-line)

      ;; Insert short description
      (let ((short-desc (python-numpydoc--prompt-for-description
                         "Short description: ")))
        (insert short-desc)
        (unless (string= (substring short-desc -1) ".")
          (insert ".")))

      ;; Insert long description if enabled
      (when python-numpydoc-prompt-for-long-description
        (newline-and-indent 2)
        (let ((long-desc (python-numpydoc--prompt-for-description
                          "Long description (optional): ")))
          (when (not (string-empty-p long-desc))
            (insert long-desc)
            (when python-numpydoc-auto-fill-paragraphs
              (fill-paragraph)))))

      ;; Insert sections
      (when parameters
        (python-numpydoc--insert-parameter-section parameters))

      (when (and return-type (not (string= return-type "None")))
        (python-numpydoc--insert-returns-section return-type))

      (when exceptions
        (python-numpydoc--insert-raises-section exceptions)))))

;;;###autoload
(defun python-numpydoc-insert-minimal ()
  "Insert a minimal NumPy-style docstring with only short description.
  Useful for simple functions that don't need extensive documentation."
  (interactive)
  (let ((python-numpydoc-prompt-for-long-description nil))
    (python-numpydoc-insert)))

(provide 'python-numpydoc)

;;; python-numpydoc.el ends here
