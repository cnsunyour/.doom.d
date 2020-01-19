;; -*- no-byte-compile: t; -*-
;;; cnsunyour/tools/packages.el


(package! symbol-overlay)
(package! pinentry)
(package! dash-at-point)
(package! posframe)
(package! alert)
(package! auto-save
  :recipe (:host github :repo "manateelazycat/auto-save"))

;; 超强的搜索框架， exec-path-from-shell 和 fuz 是其需要的工具
(package! exec-path-from-shell :disable t)
(package! fuz :disable t)
(package! snails :disable t
  :recipe (:host github :repo "manateelazycat/snails" :no-byte-compile t))
