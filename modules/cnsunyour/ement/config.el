;;; cnsunyour/ement/config.el -*- lexical-binding: t; -*-

(use-package! ement
  :defer t
  :commands
  (ement-connect)
  :bind
  ("M-g M-l" . #'ement-room-list)
  ("M-g M-r" . #'ement-room-view)
  :custom
  (ement-save-sessions t)
  :config
  (set-evil-initial-state!
    '(ement-room-list-mode
      ement-room-mode)
    'emacs)
  ;; (set-popup-rule! (regexp-quote "*Ement Rooms*")
  ;;   :side 'right :slot 10 :size 80 :ttl nil :quit 'current :modeline t)
  (set-popup-rule! (regexp-quote ement-room-buffer-name-prefix)
    :side 'right :slot 10 :size 90 :ttl 10 :quit 'current :modeline t))
