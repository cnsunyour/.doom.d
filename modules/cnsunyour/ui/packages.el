;; -*- no-byte-compile: t; -*-
;;; cnsunyour/ui/packages.el


(when IS-MAC
  (package! auto-dark-emacs
    :recipe (:host github :repo "LionyxML/auto-dark-emacs" :fork "cnsunyour/auto-dark-emacs")))
(package! awesome-tab
  :recipe (:host github :repo "manateelazycat/awesome-tab"))
(package! emojify)
(package! srcery-theme)
(package! flucui-themes)
(package! lab-themes)
