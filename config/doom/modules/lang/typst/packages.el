;; -*- no-byte-compile: t; -*-
;;; lang/typst/packages.el

(package! typst-preview
  :recipe (:host github :repo "havarddj/typst-preview.el"))
(package! websocket)
(package! typst-ts-mode
  :recipe (:type git :host codeberg :repo "meow_king/typst-ts-mode"))
