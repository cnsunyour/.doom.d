;; -*- no-byte-compile: t; -*-
;;; cnsunyour/email/packages.el

(when (featurep! :email mu4e)
  (package! mu4e-alert))
