;; -*- no-byte-compile: t; -*-
;;; cnsunyour/chinese/packages.el


;; (package! cnfonts)
(package! pangu-spacing :disable t)
(package! liberime
  :recipe (:host github :repo "DogLooksGood/liberime" :files ("CMakeLists.txt" "Makefile" "src" "*.el")))
(package! rime
  :pin "5944c530eadb806b0f940142d9cb05a9be6c8941"
  :recipe (:host github :repo "DogLooksGood/emacs-rime" :files ("rime.el")))
(package! fcitx)
(package! ace-pinyin)
(package! pinyinlib)
