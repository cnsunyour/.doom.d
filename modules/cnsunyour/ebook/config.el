;;; cnsunyour/ebook/config.el -*- lexical-binding: t; -*-


;; 阅读epub格式电子书
(use-package! nov
  :defer t
  :mode
  ("\\.epub\\'" . nov-mode)
  :custom
  (nov-text-width 80)
  :config
  (evil-set-initial-state 'nov-mode 'emacs))

(use-package! djvu
  :defer t
  :config
  (set-evil-initial-state!
    '(djvu-read-mode
      djvu-script-mode
      djvu-outline-mode
      djvu-image-mode)
    'emacs))
