;; -*- no-byte-compile: t; -*-
;;; cnsunyour/tools/packages.el


(package! pinentry)
(package! dash-at-point)
;; (package! posframe)
(package! alert)
(package! super-save)
(package! neopastebin
  :pin "4294f9c27c543718e1c24c04b47949d1fe0f4ef3"
  :recipe (:host github :repo "cnsunyour/emacs-pastebin"
           :remote "dhilst/emacs-pastebin"))
;; (package! keyfreq
;;   :recipe (:host github :repo "dacap/keyfreq"))
(package! ialign)
(package! nyan-mode
  :pin "09904af23adb839c6a9c1175349a1fb67f5b4370")
(package! atomic-chrome)
(package! git-messenger
  :pin "ae479927df8f1f79604da552930909e147605ebc"
  :recipe (:host github :repo "cnsunyour/git-messenger"
           :remote "emacsorphanage/git-messenger"))
