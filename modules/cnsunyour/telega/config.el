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
  (defadvice! +toggle-input-method--telega-chat-mode-a (chat)
    "在 telega-chat-mode 里根据 chat 名称切换输入法，如果名称包含
中文，则激活中文输入法，否则关闭中文输入法"
    :after #'telega-chat--pop-to-buffer
    (let ((input-method "pyim")
          (title (telega-chat-title chat))
          (cn-list (list "#archlinux-cn"
                         "wikipedia-zh"
                         "Jetbrains Agent"
                         "SCP-079-CHAT"))
          (en-list (list "telega.el")))
      (cond ((member title cn-list) (activate-input-method input-method))
            ((member title en-list) (activate-input-method nil))
            ((string-match "\\cc" title) (activate-input-method input-method))
            ((telega-chat-bot-p chat) (activate-input-method nil))
            ((telega-chat-private-p chat) (activate-input-method input-method))
            (t (activate-input-method nil)))))

  (set-evil-initial-state! '(telega-root-mode telega-chat-mode) 'emacs)

  (setq telega-proxies (list '(:server "127.0.0.1" :port 1086 :enable t
                                       :type (:@type "proxyTypeSocks5")))
        telega-chat-reply-prompt "<<< "
        telega-chat-edit-prompt "+++ "
        telega-sticker-size '(8 . 48)
        telega-chat-use-markdown-version nil
        telega-animation-play-inline t
        telega-emoji-use-images nil
        telega-sticker-set-download t)
  (pushnew! telega-known-inline-bots
            "@vid" "@bing" "@wiki" "@imdb")

  (set-popup-rule! (regexp-quote telega-root-buffer-name)
    :side 'right :size 100 :quit t :modeline t)
  (set-popup-rule! "^◀[[({<].*[\])}>]$"
    :side 'right :size 100 :quit t :modeline t)

  ;; Highlight current line in root buffer
  (defun lg-telega-root-mode ()
    (hl-line-mode 1))
  (defun lg-telega-chat-update (chat)
    (with-telega-root-buffer
      (hl-line-highlight)))
  (add-hook 'telega-chat-update-hook 'lg-telega-chat-update)
  (add-hook 'telega-root-mode-hook 'lg-telega-root-mode)

  ;; Outline unmuted chats using braces
  (setq telega-chat-button-brackets
        (list (list '(and (not unmuted) (type private))
                    (propertize "{" 'face 'shadow)
                    (propertize "}" 'face 'shadow))
              (list '(and (not unmuted) (type basicgroup))
                    (propertize "(" 'face 'shadow)
                    (propertize ")" 'face 'shadow))
              (list '(and (not unmuted) (type supergroup))
                    (propertize "[" 'face 'shadow)
                    (propertize "]" 'face 'shadow))
              (list '(and (not unmuted) (type channel))
                    (propertize "<" 'face 'shadow)
                    (propertize ">" 'face 'shadow))
              (list '(type private)    "{" "}")
              (list '(type basicgroup) "(" ")")
              (list '(type supergroup) "[" "]")
              (list '(type channel)    "<" ">")
              (list 'all               "[" "]")))

  ;; Links to chat/message in org-mode
  (after! org
    (defun org-telega-follow-link (link)
      (telega-tme-open-tg (concat "tg:telega:" link)))
    (defun org-telega-store-link ()
      (when-let ((link (telega-tme-internal-link-to
                        (or (telega-msg-at (point))
                            (telega-chat-at (point))))))
        ;; NOTE: strip leading "tg:"
        (let ((org-link (substring link 3)))
          (org-link-store-props :type "telega" :link org-link)
          org-link)))
    (defun org-telega-complete-link ()
      (let ((chat (telega-completing-read-chat "Chat: ")))
        (concat "telega:" (number-to-string (plist-get chat :id)))))
    (org-link-set-parameters "telega"
                             :follow 'org-telega-follow-link
                             :store 'org-telega-store-link
                             :complete 'org-telega-complete-link))


  (telega-mode-line-mode 1)
  (telega-url-shorten-mode 1)
  (when dbus-runtime-version
    (telega-notifications-mode 1))

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
