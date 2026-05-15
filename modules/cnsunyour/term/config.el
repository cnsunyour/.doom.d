;;; cnsunyour/term/config.el -*- lexical-binding: t; -*-

(use-package! ghostel
  :defer t
  :ensure t
  :init
  (map! :leader
        "o t" #'ghostel)
  :custom
  (ghostel-module-auto-install 'download)
  :config
  (set-evil-initial-state! 'ghostel-mode 'emacs))
