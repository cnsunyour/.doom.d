;;; cnsunyour/telega/config.el -*- lexical-binding: t; -*-


;; telegram client for emacs
(use-package! telega
  :commands (telega)
  :defer t
  :bind ("C-M-S-s-t" . #'telega)
  :init
  (unless (display-graphic-p) (setq telega-use-images nil))
  :hook
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
  (load! "+telega-auto-input-method")

  (set-evil-initial-state! '(telega-root-mode telega-chat-mode) 'emacs)

  (setq telega-proxies (list '(:server "127.0.0.1" :port 1086 :enable t
                                       :type (:@type "proxyTypeSocks5")))
        telega-chat-reply-prompt "<<< "
        telega-chat-edit-prompt "+++ "
        telega-sticker-size '(8 . 48)
        telega-chat-use-markdown-version nil
        telega-animation-play-inline nil
        telega-emoji-use-images nil
        telega-sticker-set-download t)
  (pushnew! telega-known-inline-bots
            "@vid" "@bing" "@wiki" "@imdb")

  (set-popup-rule! (regexp-quote telega-root-buffer-name)
    :side 'right :size 100 :quit t :modeline t)
  (set-popup-rule! "^◀[[({<].+[\])}>]"
    :side 'right :size 100 :quit t :modeline t)

  (load! "+telega-addition"))
