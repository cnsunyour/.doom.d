;; -*- no-byte-compile: t; -*-
;;; cnsunyour/tools/packages.el


(package! pinentry)
(package! dash-at-point)
(package! posframe)
(package! alert)
(package! auto-save
  :recipe (:host github :repo "manateelazycat/auto-save"))
(package! neopastebin
  :recipe (:host github :repo "dhilst/emacs-pastebin" :fork "cnsunyour/emacs-pastebin"))
