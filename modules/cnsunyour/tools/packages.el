;; -*- no-byte-compile: t; -*-
;;; cnsunyour/tools/packages.el


(package! symbol-overlay)
(package! exec-path-from-shell :disable t)
(package! pinentry)
(package! dash-at-point)
(package! posframe)
(package! alert)
(package! auto-save
  :recipe (:host github :repo "manateelazycat/auto-save"))
(package! fuz :disable t) ;; 目前只有snails在用
(package! snails :disable t)
