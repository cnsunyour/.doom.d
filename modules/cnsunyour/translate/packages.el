;; -*- no-byte-compile: t; -*-
;;; cnsunyour/translate/packages.el

(package! go-translate
  :pin "c8abe6793793401a4ec3c45553bcd75b518ecc78"
  :recipe (:host github
           :fork "cnsunyour"
           :repo "lorniu/go-translate"))
