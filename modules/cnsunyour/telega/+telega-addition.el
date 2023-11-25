;;; cnsunyour/telega/+telega-ext.el -*- lexical-binding: t; -*-

;; Highlight current line in root buffer
(defun lg-telega-root-mode ()
  (hl-line-mode))
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

;; customize some icons/stickers/urls
(setq telega-sticker-size '(8 . 48)
      telega-symbols-emojify (assq-delete-all 'checkmark telega-symbols-emojify)
      telega-symbols-emojify (assq-delete-all 'heavy-checkmark telega-symbols-emojify)
      telega-symbol-checkmark (nerd-icons-codicon "nf-cod-check")
      telega-symbol-heavy-checkmark (nerd-icons-codicon "nf-cod-check_all")
      telega-symbol-reply (nerd-icons-faicon "nf-fa-reply")
      telega-symbol-reply-quote (nerd-icons-faicon "nf-fa-reply_all")
      telega-symbol-forward (nerd-icons-mdicon "nf-md-comment_arrow_right_outline")
      telega-symbol-right-arrow (nerd-icons-codicon "nf-cod-arrow_right")
      telega-url-shorten-use-images nil)

;; add some rules to shorten urls
(add-to-list 'telega-url-shorten-regexps
             `(bilibili
               :regexp "^https?://\\(?:\\w+\\.\\)?bilibili.com/\\(.+\\)\\?.*$"
               :symbol #("󰷝" 0 1
                         (face
                          (:family "FontAwesome" :height 1.2)
                          font-lock-face
                          (:family "FontAwesome" :height 1.2)
                          display
                          (raise -0.24)
                          rear-nonsticky t))
               :replace "B站#\\1")
             t)
(add-to-list 'telega-url-shorten-regexps
             `(reddit
               :regexp "^https?://\\(?:www\\.\\)?reddit.com/\\(.+\\)/\\(.*\\)"
               :symbol #("" 0 1
                         (face
                          (:family "FontAwesome" :height 1.2)
                          font-lock-face
                          (:family "FontAwesome" :height 1.2)
                          display
                          (raise -0.24)
                          rear-nonsticky t))
               :replace "\\1/\\2")
             t)
(add-to-list 'telega-url-shorten-regexps
             `(redd.it
               :regexp "^https?://\\(?:[a-zA-Z0-9-_]+\\.\\)?redd.it/\\(.*\\)\\?.*$"
               :symbol #("󰑍" 0 1
                         (face
                          (:family "FontAwesome" :height 1.2)
                          font-lock-face
                          (:family "FontAwesome" :height 1.2)
                          display
                          (raise -0.24)
                          rear-nonsticky t))
               :replace "\\1")
             t)
(add-to-list 'telega-url-shorten-regexps
             `(emacs-china
               :regexp "^https?://\\(?:www\\.\\)?emacs-china.org/\\(.+\\)/\\(.*\\)"
               :symbol #("" 0 1
                         (face
                          (:family "FontAwesome" :height 1.2)
                          font-lock-face
                          (:family "FontAwesome" :height 1.2)
                          display
                          (raise -0.24)
                          rear-nonsticky t))
               :replace "\\1/\\2")
             t)
(add-to-list 'telega-url-shorten-regexps
             `(too-long-link
               :regexp "^https?://\\(.\\{60\\}\\).*?$"
               :symbol #("" 0 1
                         (face
                          (:family "FontAwesome" :height 1.2)
                          font-lock-face
                          (:family "FontAwesome" :height 1.2)
                          display
                          (raise -0.24)
                          rear-nonsticky t))
               :replace "\\1...")
             t)
