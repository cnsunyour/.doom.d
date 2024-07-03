;; -*- no-byte-compile: t; -*-
;;; cnsunyour/translate/packages.el

(package! go-translate
  :recipe (:host github
           :fork "cnsunyour"
           :local-repo "~/Develop/go-translate"
           :repo "lorniu/go-translate"))
