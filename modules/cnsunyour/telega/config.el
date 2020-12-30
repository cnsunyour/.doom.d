;;; cnsunyour/telega/config.el -*- lexical-binding: t; -*-


;; telegram client for emacs
(use-package! telega
  ;; :commands (telega)
  :defer t
  ;; :bind ("C-M-S-s-t" . #'telega)
  :custom
  ;; (telega-proxies (list '(:server "127.0.0.1" :port 1086 :enable t
  ;;                         :type (:@type "proxyTypeSocks5"))))
  (telega-sticker-size '(8 . 48))
  (telega-sticker-set-download t)
  (telega-use-tracking-for '(or pin unmuted mention))
  (telega-chat-show-deleted-messages-for '(or pin (me-is-owner OR-ADMIN)))
  (telega-root-fill-column 140)
  (telega-chat-fill-column 130)
  (telega-webpage-fill-column 120)
  (telega-mode-line-string-format
   '("   "
     (:eval (telega-mode-line-online-status))
     (:eval (when telega-use-tracking-for
              (telega-mode-line-tracking)))
     (:eval (telega-mode-line-unread-unmuted))
     (:eval (telega-mode-line-mentions 'messages))))
  :init
  (unless (display-graphic-p) (setq telega-use-images nil))
  (define-key global-map (kbd "C-c t") telega-prefix-map)
  (when (featurep! :completion ivy)
    (load! "+ivy-telega"))
  :hook
  (telega-chat-mode . yas-minor-mode-on)
  ;; (telega-chat-mode . visual-line-mode)
  (telega-chat-mode . (lambda ()
                        (set-company-backend! 'telega-chat-mode
                          (append '(telega-company-emoji
                                    telega-company-username
                                    telega-company-hashtag)
                                  (when (telega-chat-bot-p telega-chatbuf--chat)
                                    '(telega-company-botcmd))))))
  (telega-chat-insert-message . telega-msg-ignore-blocked-sender)
  (telega-load . telega-mode-line-mode)
  (telega-load . global-telega-squash-message-mode)
  (telega-load . global-telega-url-shorten-mode)
  (telega-load . global-telega-mnz-mode)
  :config
  (load! "+telega-auto-input-method")

  (set-evil-initial-state! '(telega-root-mode telega-chat-mode) 'emacs)

  (set-popup-rule! (regexp-quote telega-root-buffer-name)
    :side 'right :size 150 :select t :ttl t :quit 'current :modeline t)
  (set-popup-rule! "^◀[^◀\[]*[\[({<].+[\])}>]"
    :side 'right :size 150 :select t :ttl t :quit 'current :modeline t)

  ;; (if (and IS-LINUX (boundp 'dbus-runtime-version))
  ;;     (telega-notifications-mode)
  ;;   (telega-alert-mode))

  (load! "+telega-addition"))
