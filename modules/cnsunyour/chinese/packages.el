;; -*- no-byte-compile: t; -*-
;;; cnsunyour/chinese/packages.el


;; (package! cnfonts)
(package! pangu-spacing :disable t)
(package! liberime
  :recipe (:host github :repo "merrickluo/liberime" :files ("CMakeLists.txt" "Makefile" "src" "liberime.el" "liberime-config.el")))
(package! pyim)
(package! fcitx)
(package! ace-pinyin)
(package! pinyinlib)
