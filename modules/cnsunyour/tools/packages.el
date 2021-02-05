;; -*- no-byte-compile: t; -*-
;;; cnsunyour/tools/packages.el


(package! pinentry)
(package! dash-at-point)
(package! posframe)
(package! alert)
(package! auto-save
  :pin "7a9ed718f5408834a0b306251e0203a4cedfd540"
  :recipe (:host github :repo "manateelazycat/auto-save"))
(package! neopastebin
  :pin "1e088e75ae1b332f51b9bc3cd65eb3df3d3c4609"
  :recipe (:host github :repo "cnsunyour/emacs-pastebin"))
;; (package! keyfreq
;;   :pin "e5fe9d585ce882f1ba9afa5d894eaa82c79be4f4"
;;   :recipe (:host github :repo "dacap/keyfreq"))
(package! parinfer-rust-mode :pin "ca9e7b6f8c3c70daf6a933952955b6931a24af83")
