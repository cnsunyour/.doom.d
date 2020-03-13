;; -*- no-byte-compile: t; -*-
;;; cnsunyour/chinese/packages.el


;; (package! cnfonts)
(package! pangu-spacing)
(package! liberime
  :recipe (:host github :repo "merrickluo/liberime" :files ("CMakeLists.txt" "Makefile" "src" "*.el")))
(package! pyim)
(package! fcitx)
(package! ace-pinyin)
(package! pinyinlib)
