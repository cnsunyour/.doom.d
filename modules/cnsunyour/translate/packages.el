;; -*- no-byte-compile: t; -*-
;;; cnsunyour/translate/packages.el


(package! company-english-helper
  :recipe (:host github :repo "manateelazycat/company-english-helper" :files ("company-english-helper.el")))
(package! insert-translated-name
  :recipe (:host github :repo "manateelazycat/insert-translated-name"))
(package! sdcv
  :recipe (:host github :repo "manateelazycat/sdcv"))
(package! youdao-dictionary)
(package! google-translate)
