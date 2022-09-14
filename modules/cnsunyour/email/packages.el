;; -*- no-byte-compile: t; -*-
;;; cnsunyour/email/packages.el

(when (modulep! :email mu4e)
  (package! mu4e-alert))
