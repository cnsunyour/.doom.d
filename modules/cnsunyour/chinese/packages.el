;; -*- no-byte-compile: t; -*-
;;; cnsunyour/chinese/packages.el


(package! rime)
;; (package! fcitx)
(package! ace-pinyin)
(package! pinyinlib)
(package! cns
  :recipe (:host github :repo "kanglmf/emacs-chinese-word-segmentation"
           :pre-build ("make") ;; comment this line if you want to compile it in the build directory
           :files (:defaults "Makefile" "cnws*" "cppjieba")))
