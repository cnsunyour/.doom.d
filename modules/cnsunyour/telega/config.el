;;; cnsunyour/telega/config.el -*- lexical-binding: t; -*-

;; telegram client for emacs
(use-package! telega
  :defer t

  :bind-keymap*
  ("C-c t" . telega-prefix-map)

  :init
  (unless (display-graphic-p) (setq telega-use-images nil))
  (when (featurep! :completion ivy)
    (load! "+ivy-telega"))

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
  (telega-load . telega-stories-mode)

  :config
  ;; (setq telega-proxies (list '(:server "127.0.0.1" :port 1086 :enable t
  ;;                         :type (:@type "proxyTypeSocks5"))))
  (setq telega-chat-show-deleted-messages-for '(or pin (me-is-owner OR-ADMIN))
        ;; telega-use-tracking-for '(or mention (and unread unmuted))
        telega-open-file-function 'org-open-file
        ;; telega-open-message-as-file '(photo video animation)
        telega-emoji-company-backend 'telega-company-telegram-emoji
        telega-sticker-size '(8 . 48)
        telega-root-fill-column 110
        telega-chat-fill-column 100
        telega-webpage-fill-column 100
        telega-symbol-folder "📁"
        telega-chat-input-prompt '((prompt . ">>> ")
                                   (reply . "<<< ")
                                   (edit . "+++ "))
        telega-chat-input-anonymous-prompt '((prompt . "Anonymous>>> ")
                                             (reply . "Anonymous<<< ")
                                             (edit . "Anonymous+++ "))
        telega-chat-input-comment-prompt '((prompt . "Comment>>> ")
                                           (reply . "Comment<<< ")
                                           (edit . "Comment+++ "))
        telega-mode-line-string-format
        (cl-remove '(:eval (telega-mode-line-icon))
                   telega-mode-line-string-format
                   :test #'equal))

  (add-hook 'telega-msg-ignore-predicates 'telega-msg-from-blocked-sender-p)

  (map! :map telega-chat-mode-map
        "C-c C-t" #'telega-chatbuf-attach-sticker
        :map telega-root-view-map
        "e" #'telega-view-emacs-stories
        :map telega-msg-button-map
        "k" nil)

  (load! "+telega-auto-input-method")

  (set-evil-initial-state! '(telega-root-mode telega-chat-mode) 'emacs)

  (set-popup-rule! (regexp-quote telega-root-buffer-name)
    :side 'right :size 120 :select t :ttl nil :quit t :modeline t)
  (set-popup-rule! "^◀[^◀\[]*[\[({<].+[\])}>]"
    :side 'right :size 120 :select t :ttl nil :quit t :modeline t)

  (load! "+telega-addition"))
