;; -*- no-byte-compile: t; -*-
;;; cnsunyour/chinese/packages.el


;; (package! cnfonts)
(package! pangu-spacing :disable t)
(package! liberime-config
  :recipe (:host github :repo "DogLooksGood/liberime" :files ("CMakeLists.txt" "Makefile" "src" "*.el")))
(package! rime
  :recipe (:host github :repo "DogLooksGood/emacs-rime" :files ("rime.el")))
(package! fcitx)
(package! ace-pinyin)
(package! pinyinlib)
