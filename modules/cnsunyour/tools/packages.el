;; -*- no-byte-compile: t; -*-
;;; cnsunyour/tools/packages.el


(package! pinentry)
(package! dash-at-point)
;; (package! posframe)
(package! alert)
(package! super-save)
(package! neopastebin
  :pin "edde63aafaab75cb42a0496ba7c265b823ff7a44"
  :recipe (:host github :repo "cnsunyour/emacs-pastebin"
           :remote "dhilst/emacs-pastebin"))
;; (package! keyfreq
;;   :recipe (:host github :repo "dacap/keyfreq"))
(package! ialign)
(package! nyan-mode
  :pin "09904af23adb839c6a9c1175349a1fb67f5b4370")
(package! blamer)
(package! clutch)
(package! mysql)
(package! pg)
