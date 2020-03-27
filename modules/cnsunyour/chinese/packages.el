;; -*- no-byte-compile: t; -*-
;;; cnsunyour/chinese/packages.el


;; (package! cnfonts)
(package! pangu-spacing :disable t)
(package! emacs-rime
  :recipe (:host github :repo "DogLooksGood/emacs-rime"
                 ;; :fork (:host github :repo "cnsunyour/emacs-rime")
                 ;; :local-repo (expand-file-name "~/repos/emacs-rime")
                 :files ("Makefile" "lib.c" "rime.el")))
(package! fcitx)
(package! ace-pinyin)
(package! pinyinlib)
