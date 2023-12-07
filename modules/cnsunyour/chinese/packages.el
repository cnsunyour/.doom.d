;; -*- no-byte-compile: t; -*-
;;; cnsunyour/chinese/packages.el


(package! rime)
;; (package! fcitx)
(package! ace-pinyin)
(package! pinyinlib)
(package! cns
  :recipe (:host github
           :repo "kanglmf/emacs-chinese-word-segmentation"
           :pre-build ("env" "CXX=clang++" "make")))
