;;; cnsunyour/ebook/config.el -*- lexical-binding: t; -*-


;; 阅读epub格式电子书
(use-package! nov
  :defer t
  :mode
  ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-text-width 80)
  (evil-set-initial-state 'nov-mode 'emacs))
