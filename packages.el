;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:fetcher github :repo "username/repo"))
;; (package! builtin-package :disable t)

;; (package! cnfonts)
(package! pangu-spacing :disable t)
(package! pyim)
(package! fcitx)
(package! ace-pinyin)
(package! cal-china-x)
;; (package! org2blog)
(package! org-pomodoro)
(package! counsel-org-clock)
(package! company-tabnine)
(package! posframe)
(package! alert)
(package! symbol-overlay)
(package! exec-path-from-shell)
(package! youdao-dictionary
  :recipe (:host github :repo "cnsunyour/youdao-dictionary.el"))
(package! google-translate)
(package! theme-changer)
(package! pinentry)
(package! fuz)
(package! grip-mode)
;; (package! ansible)
(package! telega)
(package! weechat)

;; manateelazycat's packages
(package! company-english-helper :disable t
  :recipe (:host github :repo "manateelazycat/company-english-helper"))
(package! insert-translated-name
  :recipe (:host github :repo "manateelazycat/insert-translated-name"))
(package! sdcv
  :recipe (:host github :repo "manateelazycat/sdcv"))
(package! awesome-tab :disable t
  :recipe (:host github :repo "manateelazycat/awesome-tab"))
(package! aweshell :disable t
  :recipe (:host github :repo "manateelazycat/aweshell"))
(package! awesome-pair :disable t
  :recipe (:host github :repo "manateelazycat/awesome-pair"))
(package! highlight-matching-tag :disable t
  :recipe (:host github :repo "manateelazycat/highlight-matching-tag"))
(package! instant-rename-tag :disable t
  :recipe (:host github :repo "manateelazycat/instant-rename-tag"))
;; (package! snails
;;   :recipe (:host github :repo "manateelazycat/snails"))
