;; -*- no-byte-compile: t; -*-
;;; cnsunyour/translate/packages.el

(package! names)
(package! youdao-dictionary)
(package! go-translate
  :recipe (:host github
           :fork "cnsunyour"
           :local-repo "~/Develop/go-translate"
           :repo "lorniu/go-translate"))
