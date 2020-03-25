;; -*- no-byte-compile: t; -*-
;;; cnsunyour/chinese/packages.el


;; (package! cnfonts)
(package! pangu-spacing :disable t)
(package! emacs-rime
  :recipe (:host github :repo "DogLooksGood/emacs-rime" :files ("Makefile" "lib.c" "rime.el")))
                 ;; :fork (:host github :repo "cnsunyour/emacs-rime")))
(package! fcitx)
(package! ace-pinyin)
(package! pinyinlib)
