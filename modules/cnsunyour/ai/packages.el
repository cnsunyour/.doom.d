;; -*- no-byte-compile: t; -*-
;;; cnsunyour/tools/packages.el

(package! gptel)
(package! gptel-extensions
  :recipe (:host github :repo "kamushadenes/gptel-extensions.el"))
(package! org-ai
  :recipe (:host github :repo "rksm/org-ai"
           :files (:defaults "snippets")))
(package! magit-gptcommit)
(package! codeium
  :recipe (:host github :repo "Exafunction/codeium.el"))
(package! aidermacs)
