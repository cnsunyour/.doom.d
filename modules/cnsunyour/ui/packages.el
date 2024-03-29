;; -*- no-byte-compile: t; -*-
;;; cnsunyour/ui/packages.el

(unless IS-MAC
  (package! theme-changer
    :pin "41619712074158cc284a919377ad12d7ab528f5c"
    :recipe (:host github :repo "cnsunyour/theme-changer"
             :remote "hadronzoo/theme-changer")))
(when (and IS-MAC (not (boundp 'ns-system-appearance-change-functions)))
  (package! auto-dark-emacs
    :pin "e4d20c3b0e6d83101d615807e0adc0203ebb1bc7"
    :recipe (:host github :repo "cnsunyour/auto-dark-emacs"
             :remote "LionyxML/auto-dark-emacs")))
