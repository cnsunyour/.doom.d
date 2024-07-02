;; -*- no-byte-compile: t; -*-
;;; cnsunyour/tools/packages.el

(package! gptel)
(package! gptel-extensions
  :recipe (:host github :repo "kamushadenes/gptel-extensions.el"))
(package! ai-blog
  :recipe (:host github :repo "kamushadenes/ai-blog.el"))
(package! org-ai
  :recipe (:host github :repo "rksm/org-ai"
           :files (:defaults "snippets")))
(package! magit-gptcommit)
(package! llm
  :recipe (:host github
           :fork "cnsunyour"
           :local-repo "~/Develop/llm"
           :repo "ahyatt/llm"))
