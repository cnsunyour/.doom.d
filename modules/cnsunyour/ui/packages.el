;; -*- no-byte-compile: t; -*-
;;; cnsunyour/ui/packages.el

(unless IS-MAC
  (package! theme-changer
    :pin "41619712074158cc284a919377ad12d7ab528f5c"
    :recipe (:host github :repo "cnsunyour/theme-changer")))
(when IS-MAC
  (package! auto-dark-emacs
    :pin "06268b907d269a68922536ae00a22e023e54ca8d"
    :recipe (:host github :repo "cnsunyour/auto-dark-emacs")))
(package! awesome-tab
  :pin "5f2c76d5f889991baca6ee8e823c1c9dceeec05b"
  :recipe (:host github :repo "manateelazycat/awesome-tab"))
(package! emojify)
