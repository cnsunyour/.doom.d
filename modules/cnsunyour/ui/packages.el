;; -*- no-byte-compile: t; -*-
;;; cnsunyour/ui/packages.el

(unless IS-MAC
  (package! theme-changer
    :recipe (:host github :repo "cnsunyour/theme-changer")))
(when IS-MAC
  (package! auto-dark-emacs
    :recipe (:host github :repo "cnsunyour/auto-dark-emacs")))
(package! awesome-tab
  :recipe (:host github :repo "manateelazycat/awesome-tab"))
(package! emojify)
