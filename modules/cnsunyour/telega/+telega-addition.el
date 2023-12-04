;;; cnsunyour/telega/+telega-addition.el -*- lexical-binding: t; -*-

;; Highlight current line in root buffer
(defun lg-telega-root-mode ()
  (hl-line-mode))
(defun lg-telega-chat-update (chat)
  (with-telega-root-buffer
    (hl-line-highlight)))
(add-hook 'telega-root-mode-hook 'lg-telega-root-mode)
(add-hook 'telega-chat-update-hook 'lg-telega-chat-update)

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
             `(youtube-short :regexp "^https?://www.youtube.com/shorts/\\([^?]+\\).+"
               :symbol #("" 0 1
                         (face
                          (:family "FontAwesome" :height 1.2)
                          font-lock-face
                          (:family "FontAwesome" :height 1.2)
                          display
                          (raise -0.24)
                          rear-nonsticky t))
               :replace "YouTube#\\1"
               :svg-icon ("fa-brands/youtube-rgb.svg" :scale 0.6))
             t)
(add-to-list 'telega-url-shorten-regexps
             `(github-gist
               :regexp "^https?://gist.github.com/\\(.+\\)/\\(.+[^/?]\\)[/?]?"
               :symbol ""
               :replace "Gist:\\1/\\2"
               :svg-icon ("fa-brands/github-octocat.svg" :scale 0.72))
             t)
(add-to-list 'telega-url-shorten-regexps
             `(gnu-mailing-list-archive
               :regexp "^https?://lists.gnu.org/archive/html/\\([^?]+\\)\\(?:\\?.*\\)?$"
               :symbol ""
               :replace "\\1")
             t)
(add-to-list 'telega-url-shorten-regexps
             `(twitter
               :regexp "^https?://\\(?:www.\\)?\\(?:twitter\\|x\\).com/\\(.+\\)/status/\\([^?]+\\)\\(?:\\?.*\\)?$"
               :symbol "󰕄"
               :replace "\\1#\\2")
             t)
(add-to-list 'telega-url-shorten-regexps
             `(facebook
               :regexp "^https?://\\(?:www.\\)?facebook.com/\\([^?]+\\)\\(?:\\?.*\\)?$"
               :symbol "󰈌"
               :replace "\\1")
             t)
(add-to-list 'telega-url-shorten-regexps
             `(bilibili-video
               :regexp "^https?://\\(?:\\w+.\\)?bilibili.com/video/\\(.+[^/?]\\)[/?]?"
               :symbol "󰷝"
               :replace "B站#\\1")
             t)
(add-to-list 'telega-url-shorten-regexps
             `(bilibili
               :regexp "^https?://\\(?:\\w+.\\)?bilibili.com/\\([^?]+\\)\\(?:\\?.*\\)?$"
               :symbol "󰷝"
               :replace "B站:\\1")
             t)
(add-to-list 'telega-url-shorten-regexps
             `(reddit
               :regexp "^https?://\\(?:www.\\)?reddit.com/\\([^?]+\\)\\(?:\\?.*\\)?$"
               :symbol ""
               :replace "\\1")
             t)
(add-to-list 'telega-url-shorten-regexps
             `(redd.it
               :regexp "^https?://\\(?:\\w+.\\)?redd.it/\\([^?]+\\)\\(?:\\?.*\\)?$"
               :symbol ""
               :replace "\\1")
             t)
(add-to-list 'telega-url-shorten-regexps
             `(stackoverflow
               :regexp "^https?://\\(?:\\w+.\\)?stackoverflow.com/questions/\\([^?]+\\)\\(?:\\?.*\\)?$"
               :symbol ""
               :replace "#\\1")
             t)
(add-to-list 'telega-url-shorten-regexps
             `(stackexchange
               :regexp "^https?://\\(?:\\w+.\\)?stackexchange.com/questions/\\([^?]+\\)\\(?:\\?.*\\)?$"
               :symbol ""
               :replace "#\\1")
             t)
(add-to-list 'telega-url-shorten-regexps
             `(telegram
               :regexp "^https?://t.me/\\(.+\\)$"
               :symbol ""
               :replace "tg:\\1")
             t)
(add-to-list 'telega-url-shorten-regexps
             `(emacs-china
               :regexp "^https?://\\(?:www.\\)?emacs-china.org/\\([^?]+\\)\\(?:\\?.*\\)?$"
               :symbol ""
               :replace "\\1")
             t)
(add-to-list 'telega-url-shorten-regexps
             `(too-long-link
               :regexp "^https?://\\(?:www.\\)?\\(.\\{60\\}\\).*?$"
               :symbol ""
               :replace "\\1...")
             t)
(add-to-list 'telega-url-shorten-regexps
             `(other-link
               :regexp "^https?://\\(?:www.\\)?\\(.+[^/?]\\)[/?]?"
               :symbol ""
               :replace "\\1")
             t)
