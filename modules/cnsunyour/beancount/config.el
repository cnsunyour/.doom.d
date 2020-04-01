;;; cnsunyour/beancount/config.el -*- lexical-binding: t; -*-


;; beancount复式账簿记账
(use-package! beancount
  :defer t
  :bind
  ("C-M-S-s-b" . (lambda() (interactive) (find-file "~/Dropbox/beancount/main.bean")))
  :mode
  ("\\.bean\\(?:count\\)?\\'" . beancount-mode)
  :hook
  ('beancount-mode . #'yas-minor-mode-on)
  :custom
  (beancount-accounts-files (directory-files "~/Dropbox/beancount/accounts" 'full (rx ".bean" eos))))
