;; -*- no-byte-compile: t; -*-
;;; cnsunyour/chinese/packages.el


(package! rime)
  ;; :recipe (:host github :repo "DogLooksGood/emacs-rime"
  ;;                ;; :fork "cnsunyour/emacs-rime"
  ;;                ;; :local-repo "~/repos/emacs-rime"
  ;;                :files ("Makefile" "lib.c" "*.el")))
(package! fcitx :pin "12dc2638ddd15c8f6cfaecb20e1f428ab2bb5624")
(package! ace-pinyin :pin "8b2e9335b02486730ea4ceee790130cc5328f9ea")
(package! pinyinlib :pin "1772c79b6f319b26b6a394a8dda065be3ea4498d")
