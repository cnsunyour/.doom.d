;; -*- no-byte-compile: t; -*-
;;; cnsunyour/colipot/packages.el

(package! copilot
  :recipe (:host github
           :repo "zerolfx/copilot.el"
           :files ("*.el" "dist")))
