;; -*- no-byte-compile: t; -*-
;;; lang/cylc/packages.el

(package! cylc-mode
  :recipe (:host github :repo "cylc/cylc-flow" :files ("cylc/flow/etc/syntax/cylc-mode.el")))
