;; -*- no-byte-compile: t; -*-
;;; cnsunyour/tools/packages.el

(package! org-ai
  :recipe (:host github :repo "rksm/org-ai"
           :files (:defaults "snippets")))
(package! claude-code-ide
  :recipe (:host github :repo "manzaltu/claude-code-ide.el"))
