;;; cnsunyour/beancount/config.el -*- lexical-binding: t; -*-


;; beancount复式账簿记账
(use-package! beancount
  :defer t
  :mode
  ("\\.bean\\(?:count\\)?\\'" . beancount-mode)
  :hook
  (beancount-mode . yas-minor-mode-on)
  :config
  (setq beancount-accounts-files (directory-files "~/Dropbox/beancount/accounts" 'full (rx ".bean" eos))))
