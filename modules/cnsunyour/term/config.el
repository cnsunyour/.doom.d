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
  (set-popup-rule! "^\\*ghostel" :size 0.25 :vslot -4 :select t :quit nil :ttl 0)
  (set-evil-initial-state! 'ghostel-mode 'emacs))
