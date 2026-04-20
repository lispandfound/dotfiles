;;; lang/just/config.el -*- lexical-binding: t; -*-
(cl-defun just-formatter (&key scratch callback &allow-other-keys)
  "Format a Justfile using a temporary file because just --fmt refuses stdin."
  (let ((temp (make-temp-file "justfile-")))
    (with-current-buffer scratch
      (write-region (point-min) (point-max) temp nil 'silent))
    
    (if (zerop (call-process "just" nil nil nil "--unstable" "--fmt" "-j" temp))
        (with-current-buffer scratch
          (erase-buffer)
          (insert-file-contents temp)
          (delete-file temp)
          (funcall callback nil))
      
      (delete-file temp)
      (funcall callback '(error "Just formatting failed")))))

(use-package! just-ts-mode

  :config
  (set-formatter! 'just-format
    #'just-formatter
    :modes '(just-ts-mode)))
