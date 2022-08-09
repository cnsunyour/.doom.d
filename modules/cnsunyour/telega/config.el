;;; cnsunyour/telega/config.el -*- lexical-binding: t; -*-

;; telegram client for emacs
(use-package! telega
  :defer t

  :bind-keymap*
  ("C-c t" . telega-prefix-map)

  :init
  (unless (display-graphic-p) (setq telega-use-images nil))
  (pushnew! +evil-collection-disabled-list 'telega)

  :hook
  ;; (telega-chat-mode . yas-minor-mode-on)
  ;; (telega-chat-mode . visual-line-mode)
  (telega-chat-mode . (lambda ()
                        (set-company-backend! 'telega-chat-mode
                          (append (list telega-emoji-company-backend
                                        'telega-company-username
                                        'telega-company-hashtag)
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
        ;; telega-chat-fill-column 78
        telega-root-fill-column (+ telega-chat-fill-column 10)
        telega-root-auto-fill-mode nil
        telega-symbol-folder "📁"
        telega-symbol-reply "←"
        telega-mode-line-string-format '((:eval
                                          (car
                                           (telega-account-current)))
                                         (:eval
                                          (telega-mode-line-online-status))
                                         (:eval
                                          (telega-mode-line-unread-unmuted))
                                         (:eval
                                          (telega-mode-line-mentions 'messages))))

  (when (featurep! :completion ivy) (load! "+ivy-telega"))

  (map! :map telega-chat-mode-map
        "C-c C-t" #'telega-chatbuf-attach-sticker
        :map telega-msg-button-map
        "k" nil)

  (load! "+telega-auto-input-method")

  (set-evil-initial-state!
    '(telega-root-mode
      telega-chat-mode
      telega-image-mode)
    'emacs)

  (set-popup-rule! (regexp-quote telega-root-buffer-name)
    :side 'right :size (+ telega-chat-fill-column 13) :ttl nil :quit 'current :modeline t)
  (set-popup-rule! "^◀[^◀\[]*[\[({<].+[\])}>]"
    :side 'right :size (+ telega-chat-fill-column 13) :ttl 10 :quit 'current :modeline t)
  (set-popup-rule! (regexp-quote "*Telega Instant View*")
    :side 'right :slot 10 :width telega-webpage-fill-column :height 0.5 :ttl 10 :quit t :modeline nil)

  (load! "+telega-addition"))
