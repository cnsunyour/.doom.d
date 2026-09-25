;; -*- no-byte-compile: t; -*-
;;; cnsunyour/tools/packages.el

(package! gptel
  :recipe (:nonrecursive t))
(package! org-ai
  :recipe (:host github :repo "rksm/org-ai"
           :files (:defaults "snippets")))
(package! gptel-magit
  :recipe (:host github :repo "roife/gptel-magit"))
(package! claude-code-ide
  :recipe (:host github :repo "manzaltu/claude-code-ide.el"))
