(require 'transient)
(require 'seq)

(defvar uv--parent-args nil
  "Temporary storage for arguments inherited from the parent transient.")

;;;###autoload
(defun uv--run (subcommand &optional sub-args)
  "The final execution engine.
Combines subcommand, sub-args, and the inherited parent-args."
  (let* (;; Combine inherited globals with local sub-menu args
         (all-args (append uv--parent-args subcommand sub-args))
         ;; Filter out nils and join into a shell string
         (cmd (string-join (cons "uv" (seq-filter #'stringp all-args)) " ")))
    (message "Running: %s" cmd)
    (detached-compile cmd)))


;;;###autoload
(transient-define-prefix uv-add-menu ()
  "Sub-menu for 'uv add'. Inherits global flags."
  ["Add Arguments"
   ("-D" "Dev dependency" "--dev")
   ("-o" "Optional group" "--optional=" :prompt "Group name: ")
   ("-r" "Raw sources"    "--raw-sources")]
  ["Actions"
   ("a" "Confirm Add"
    (lambda (args)
      (interactive (list (transient-args 'uv-add-menu)))
      (let ((pkg (read-string "Package(s) to add: ")))
        (uv--run '("add") (append args (list pkg))))))])

;;;###autoload
(transient-define-prefix uv-run-menu ()
  "Sub-menu for 'uv run'. Inherits global flags."
  ["Run Arguments"
   ("-m" "Module"          "--module")
   ("-p" "Python version"  "--python=" :prompt "Version: ")
   ("-w" "With package"    "--with="   :prompt "Package: ")]
  ["Actions"
   ("x" "Execute Run"
    (lambda (args)
      (interactive (list (transient-args 'uv-run-menu)))
      (let ((cmd (read-string "Command/Script: ")))
        (uv--run '("run") (append args (list cmd))))))])

;;;###autoload
(transient-define-prefix uv-pip-menu ()
  "Sub-menu for 'uv pip'. Inherits global flags."
  ["Actions"
   ("i" "Install" (lambda () (interactive) (uv--run '("pip" "install") (list (read-string "Package: ")))))
   ("l" "List"    (lambda () (interactive) (uv--run '("pip" "list") nil)))
   ("f" "Freeze"  (lambda () (interactive) (uv--run '("pip" "freeze") nil)))] )

;;;###autoload
(transient-define-prefix uv-python-menu ()
  "Sub-menu for 'uv python'. Inherits global flags."
  ["Actions"
   ("l" "List"      (lambda () (interactive) (uv--run '("python" "list") nil)))
   ("i" "Install"   (lambda () (interactive) (uv--run '("python" "install") (list (read-string "Version: ")))))
   ("u" "Uninstall" (lambda () (interactive) (uv--run '("python" "uninstall") (list (read-string "Version: ")))))] )

;;;###autoload
(transient-define-prefix uv-menu ()
  "Main Transient router for uv."
  [:description "uv (Astral) - Global Settings"
                ["Global Flags (Inherited by sub-menus)"
                 ("-n" "No Cache"       "--no-cache")
                 ("-o" "Offline"        "--offline")
                 ("-v" "Verbose"        "--verbose")
                 ("--" "No Progress"    "--no-progress")]
                ["Global Flags (Python)"
                 ("m" "Managed Python"  "--managed-python")
                 ("M" "No Managed Py"   "--no-managed-python")
                 ("D" "No Downloads"    "--no-python-downloads")]]

  [["Project"
    ("s" "Sync"      (lambda () (interactive) (setq uv--parent-args (transient-args 'uv-menu)) (uv--run '("sync"))))
    ("l" "Lock"      (lambda () (interactive) (setq uv--parent-args (transient-args 'uv-menu)) (uv--run '("lock"))))
    ("i" "Init"      (lambda () (interactive) (setq uv--parent-args (transient-args 'uv-menu)) (uv--run '("init") (list (read-string "Project Name: ")))))
    ("t" "Tree"      (lambda () (interactive) (setq uv--parent-args (transient-args 'uv-menu)) (uv--run '("tree"))))
    ("F" "Format"    (lambda () (interactive) (setq uv--parent-args (transient-args 'uv-menu)) (uv--run '("format"))))]

   ["Manage"
    ("a" "Add..."    (lambda () (interactive) (setq uv--parent-args (transient-args 'uv-menu)) (uv-add-menu)))
    ("r" "Remove"    (lambda () (interactive) (setq uv--parent-args (transient-args 'uv-menu)) (uv--run '("remove") (list (read-string "Package: ")))))
    ("E" "Export"    (lambda () (interactive) (setq uv--parent-args (transient-args 'uv-menu)) (uv--run '("export") nil)))
    ("b" "Build"     (lambda () (interactive) (setq uv--parent-args (transient-args 'uv-menu)) (uv--run '("build") nil)))]

   ["Execution"
    ("x" "Run..."    (lambda () (interactive) (setq uv--parent-args (transient-args 'uv-menu)) (uv-run-menu)))
    ("P" "Pip..."    (lambda () (interactive) (setq uv--parent-args (transient-args 'uv-menu)) (uv-pip-menu)))
    ("y" "Python..." (lambda () (interactive) (setq uv--parent-args (transient-args 'uv-menu)) (uv-python-menu)))
    ("v" "Venv"      (lambda () (interactive) (setq uv--parent-args (transient-args 'uv-menu)) (uv--run '("venv") nil)))]

   ["System"
    ("C" "Cache"     (lambda () (interactive) (setq uv--parent-args (transient-args 'uv-menu)) (uv--run '("cache" "clean") nil)))
    ("S" "Self"      (lambda () (interactive) (setq uv--parent-args (transient-args 'uv-menu)) (uv--run '("self" "update") nil)))
    ("h" "Help"      (lambda () (interactive) (setq uv--parent-args (transient-args 'uv-menu)) (uv--run '("help") nil)))]]

  (interactive)
  (transient-setup 'uv-menu))

(provide 'uv)
