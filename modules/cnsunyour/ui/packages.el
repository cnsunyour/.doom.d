;; -*- no-byte-compile: t; -*-
;;; cnsunyour/ui/packages.el

(unless IS-MAC
  (package! theme-changer
    :pin "41619712074158cc284a919377ad12d7ab528f5c"
    :recipe (:host github :repo "cnsunyour/theme-changer")))
(when IS-MAC
  (package! auto-dark-emacs
    :pin "fede6f4c7ae87a0abc50591e2b29ed0666c1522e"
    :recipe (:host github :repo "cnsunyour/auto-dark-emacs")))
(package! awesome-tab
  :pin "5f2c76d5f889991baca6ee8e823c1c9dceeec05b"
  :recipe (:host github :repo "manateelazycat/awesome-tab"))
(package! emojify)
