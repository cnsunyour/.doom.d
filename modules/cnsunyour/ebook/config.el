;;; cnsunyour/ebook/config.el -*- lexical-binding: t; -*-


;; 阅读epub格式电子书
(use-package! nov
  :defer t
  :mode
  ("\\.epub\\'" . nov-mode)
  :config
  ;; (evil-set-initial-state 'nov-mode 'emacs)
  (map! :map nov-mode-map
        :n "q" #'+popup/quit-window))
