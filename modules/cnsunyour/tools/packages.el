;; -*- no-byte-compile: t; -*-
;;; cnsunyour/tools/packages.el


(package! pinentry)
(package! dash-at-point)
(package! posframe)
(package! alert)
(package! auto-save
  :recipe (:host github :repo "manateelazycat/auto-save"))
(package! neopastebin
  :recipe (:host github :repo "dhilst/emacs-pastebin" :fork "cnsunyour/emacs-pastebin"))

;; 超强的搜索框架， exec-path-from-shell 和 fuz 是其需要的工具
(package! exec-path-from-shell :disable t)
(package! fuz)
(package! snails
  :recipe (:host github :repo "manateelazycat/snails" :no-byte-compile t
                 :files ("*.el" "*.sh" "*.ps1")))
