;;; cnsunyour/telega/config.el -*- lexical-binding: t; -*-


;; telegram client for emacs
(use-package! telega
  :commands (telega)
  :defer t
  :bind ("C-M-S-s-t" . #'telega)
  :init
  (unless (display-graphic-p) (setq telega-use-images nil))
  :hook
  ('telega-chat-mode . #'toggle-input-method) ;; 激活输入法必须在hook列表的最后才有效
  ('telega-root-mode . #'evil-emacs-state)
  ('telega-chat-mode . #'evil-emacs-state)
  ('telega-chat-mode . #'yas-minor-mode-on)
  ('telega-chat-mode . (lambda ()
                         (set-company-backend! 'telega-chat-mode
                           (append '(telega-company-emoji
                                     telega-company-username
                                     telega-company-hashtag)
                                   (when (telega-chat-bot-p telega-chatbuf--chat)
                                     '(telega-company-botcmd))))))
  ('telega-chat-pre-message . #'telega-msg-ignore-blocked-sender)
  :config
  (setq telega-proxies (list '(:server "127.0.0.1" :port 1086 :enable t
                                       :type (:@type "proxyTypeSocks5")))
        telega-chat-use-markdown-formatting nil
        telega-animation-play-inline t
        telega-use-tracking t
        telega-emoji-use-images nil
        telega-sticker-set-download t
        telega-chat-footer-show-pinned-message nil)
  (set-popup-rule! (regexp-quote telega-root-buffer-name)
    :side 'right :size 100 :quit t :modeline t)
  (set-popup-rule! "^◀[[({<].*[\])}>]$"
    :side 'right :size 100 :quit t :modeline t)
  (telega-mode-line-mode 1)
  (telega-url-shorten-mode 1)
  (when (featurep! :completion ivy)
    (load! "+ivy-telega"))
  (after! all-the-icons
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(telega-root-mode all-the-icons-fileicon "telegram"
                                    :heigt 1.0
                                    :v-adjust -0.2
                                    :face all-the-icons-yellow))
    (add-to-list 'all-the-icons-mode-icon-alist
                 '(telega-chat-mode all-the-icons-fileicon "telegram"
                                    :heigt 1.0
                                    :v-adjust -0.2
                                    :face all-the-icons-blue))))
