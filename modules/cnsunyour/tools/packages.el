;; -*- no-byte-compile: t; -*-
;;; cnsunyour/tools/packages.el


(package! pinentry)
(package! dash-at-point)
;; (package! posframe)
(package! alert)
(package! super-save)
(package! neopastebin
  :pin "1e088e75ae1b332f51b9bc3cd65eb3df3d3c4609"
  :recipe (:host github :repo "cnsunyour/emacs-pastebin"))
;; (package! keyfreq
;;   :recipe (:host github :repo "dacap/keyfreq"))
(package! ialign)
(package! nyan-mode
  :pin "09904af23adb839c6a9c1175349a1fb67f5b4370")
(package! atomic-chrome)
(package! git-messenger
  :pin "063efe38b23894efe081917d8058869cff03393b"
  :recipe (:host github :repo "cnsunyour/git-messenger"
           :local-repo "~/Develop/git-messenger"
           :remote "emacsorphanage/git-messenger"))
