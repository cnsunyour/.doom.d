;;; cnsunyour/telega/config.el -*- lexical-binding: t; -*-

;; telegram client for emacs
(use-package! telega
  :defer t

  :init
  (define-key global-map (kbd "C-c t") telega-prefix-map)
  (unless (display-graphic-p) (setq telega-use-images nil))
  (pushnew! +evil-collection-disabled-list 'telega)

  :hook
  ;; (telega-chat-mode . yas-minor-mode-on)
  ;; (telega-chat-mode . visual-line-mode)
  (telega-chat-mode . (lambda ()
                        (set-company-backend! 'telega-chat-mode
                          (append (list telega-emoji-company-backend
                                        'telega-company-username
                                        'telega-company-hashtag
                                        'telega-company-markdown-precode)
                                  (when (telega-chat-bot-p telega-chatbuf--chat)
                                    '(telega-company-botcmd))))))
  (telega-load . telega-mode-line-mode)
  (telega-load . global-telega-url-shorten-mode)
  (telega-load . global-telega-mnz-mode)
  (telega-load . telega-autoplay-mode)

  :config
  (add-hook 'telega-msg-ignore-predicates
            (telega-match-gen-predicate "msg-" '(sender blocked)))
  ;; (setq telega-proxies (list '(:server "127.0.0.1" :port 1086 :enable t
  ;;                         :type (:@type "proxyTypeSocks5"))))
  (setq telega-chat-show-deleted-messages-for '(me-is-owner OR-ADMIN)
        ;; telega-use-tracking-for '(or mention (and unread unmuted))
        telega-open-file-function 'org-open-file
        ;; telega-open-message-as-file '(video video-note)
        telega-emoji-company-backend 'telega-company-telegram-emoji
        telega-sticker-size '(8 . 48)
        ;; telega-chat-fill-column 88
        telega-root-fill-column (+ telega-chat-fill-column 10)
        telega-root-auto-fill-mode nil
        telega-translate-to-language-by-default "zh"
        telega-mode-line-string-format (remove
                                        '(:eval (telega-mode-line-icon))
                                        telega-mode-line-string-format))

  (when (modulep! :completion ivy) (load! "+ivy-telega"))

  (map! :map telega-chat-mode-map
        "C-c C-e" #'telega-chatbuf-attach-sticker
        "C-c C-t" #'telega-auto-translate-mode)

  (load! "+telega-auto-input-method")

  (set-evil-initial-state!
    '(telega-root-mode
      telega-chat-mode
      telega-image-mode)
    'emacs)

  (set-popup-rule! (regexp-quote telega-root-buffer-name)
    :slot 10 :vslot 10 :side 'right :size (+ telega-root-fill-column 3) :ttl nil :quit 'current :modeline t)
  (set-popup-rule! "^◀[^◀\[]*[\[({<].+[\])}>]"
    :slot 10 :vslot 10 :side 'right :size (+ telega-chat-fill-column 13) :ttl 10 :quit 'current :modeline t)
  (set-popup-rule! (regexp-quote "*Telega Instant View*")
    :slot 20 :vslot 10 :side 'right :height .5 :ttl 10 :quit t :modeline nil :select t)
  (set-popup-rule! (regexp-quote "*Telega User*")
    :slot 20 :vslot 10 :side 'right :height .5 :ttl 10 :quit t :modeline nil :select t)
  (set-popup-rule! (regexp-quote "*Telegram Chat Info*")
    :slot 20 :vslot 10 :side 'right :height .5 :ttl 10 :quit t :modeline nil :select t)

  (load! "+telega-addition"))
