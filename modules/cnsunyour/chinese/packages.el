;; -*- no-byte-compile: t; -*-
;;; cnsunyour/chinese/packages.el


;; (package! cnfonts)
(package! pangu-spacing :disable t)
(package! rime
  :recipe (:host github :repo "DogLooksGood/emacs-rime"
                 ;; :fork (:host github :repo "cnsunyour/emacs-rime")
                 ;; :local-repo "~/repos/emacs-rime"
                 :files ("Makefile" "lib.c" "*.el")))
(package! fcitx)
(package! ace-pinyin)
(package! pinyinlib)
