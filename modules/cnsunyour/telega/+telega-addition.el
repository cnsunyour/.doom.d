;;; cnsunyour/telega/+telega-addition.el -*- lexical-binding: t; -*-

;; Highlight current line in root buffer
(defun lg-telega-root-mode ()
  (hl-line-mode))
(defun lg-telega-chat-update (chat)
  (with-telega-root-buffer
    (hl-line-highlight)))
(add-hook 'telega-root-mode-hook 'lg-telega-root-mode)
(add-hook 'telega-chat-update-hook 'lg-telega-chat-update)

(define-advice telega-chatbuf--sponsored-messages-fetch (:override (&rest _) dont-fetch-sponsor-a))

(define-advice telega-etc-file (:override (filename) check-custom-etc-path-a)
  "Check if FILENAME exists in custom etc/ directory."
  (let ((new-path (expand-file-name (concat "etc/" filename) doom-user-dir)))
    ;; Check if a file exists at the new path
    (if (file-exists-p new-path)
        ;; If it does, return new-path.
        new-path
      ;; If it does not, fall back to the original function telega-etc-file.
      ;; advice--cd*r is an internal function used for calling the original unadvised function
      ;; from within an advice. Here, it is used to fall back to the original behavior of telega-etc-file.
      (funcall (advice--cd*r (indirect-function 'telega-etc-file)) filename))))

;; customize some icons/stickers/urls
(setq
      ;; telega-symbols-emojify (assq-delete-all 'checkmark telega-symbols-emojify)
      ;; telega-symbols-emojify (assq-delete-all 'heavy-checkmark telega-symbols-emojify)
      ;; telega-symbol-checkmark (nerd-icons-codicon "nf-cod-check")
      ;; telega-symbol-heavy-checkmark (nerd-icons-codicon "nf-cod-check_all")
      ;; telega-symbol-reply (nerd-icons-faicon "nf-fa-reply")
      ;; telega-symbol-reply-quote (nerd-icons-faicon "nf-fa-reply_all")
      ;; telega-symbol-forward (nerd-icons-mdicon "nf-md-comment_arrow_right_outline")
      ;; telega-symbol-right-arrow (nerd-icons-codicon "nf-cod-arrow_right")
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
               :symbol #("" 0 1
                         (face
                          (:family "FontAwesome" :height 1.2)
                          font-lock-face
                          (:family "FontAwesome" :height 1.2)
                          display
                          (raise -0.24)
                          rear-nonsticky t))
               :replace "Gist:\\1/\\2"
               :svg-icon ("fa-brands/github-octocat.svg" :scale 0.72))
             t)
(add-to-list 'telega-url-shorten-regexps
             `(gnu-mailing-list-archive
               :regexp "^https?://lists.gnu.org/archive/html/\\([^?]+\\)\\(?:\\?.*\\)?$"
               :symbol #("" 0 1
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
             `(twitter
               :regexp "^https?://\\(?:www.\\)?\\(?:twitter\\|x\\).com/\\(.+\\)/status/\\([^?]+\\)\\(?:\\?.*\\)?$"
               :symbol #("󰕄" 0 1
                         (face
                          (:family "FontAwesome" :height 1.2)
                          font-lock-face
                          (:family "FontAwesome" :height 1.2)
                          display
                          (raise -0.24)
                          rear-nonsticky t))
               :replace "\\1#\\2"
               :svg-icon ("fa-brands/twitter.svg" :scale 0.72))
             t)
(add-to-list 'telega-url-shorten-regexps
             `(facebook
               :regexp "^https?://\\(?:www.\\)?facebook.com/\\([^?]+\\)\\(?:\\?.*\\)?$"
               :symbol #("󰈌" 0 1
                         (face
                          (:family "FontAwesome" :height 1.2)
                          font-lock-face
                          (:family "FontAwesome" :height 1.2)
                          display
                          (raise -0.24)
                          rear-nonsticky t))
               :replace "\\1"
               :svg-icon ("fa-brands/facebook.svg" :scale 0.72))
             t)
(add-to-list 'telega-url-shorten-regexps
             `(bilibili-video
               :regexp "^https?://\\(?:\\w+.\\)?bilibili.com/video/\\(.+[^/?]\\)[/?]?"
               :symbol #("󰟴" 0 1
                         (face
                          (:family "FontAwesome" :height 1.2)
                          font-lock-face
                          (:family "FontAwesome" :height 1.2)
                          display
                          (raise -0.24)
                          rear-nonsticky t))
               :replace "B站#\\1"
               :svg-icon ("fa-brands/bilibili.svg" :scale 0.72))
             t)
(add-to-list 'telega-url-shorten-regexps
             `(bilibili
               :regexp "^https?://\\(?:\\w+.\\)?bilibili.com/\\([^?]+\\)\\(?:\\?.*\\)?$"
               :symbol #("󰟴" 0 1
                         (face
                          (:family "FontAwesome" :height 1.2)
                          font-lock-face
                          (:family "FontAwesome" :height 1.2)
                          display
                          (raise -0.24)
                          rear-nonsticky t))
               :replace "B站:\\1"
               :svg-icon ("fa-brands/bilibili.svg" :scale 0.72))
             t)
(add-to-list 'telega-url-shorten-regexps
             `(reddit
               :regexp "^https?://\\(?:www.\\)?reddit.com/\\([^?]+\\)\\(?:\\?.*\\)?$"
               :symbol #("" 0 1
                         (face
                          (:family "FontAwesome" :height 1.2)
                          font-lock-face
                          (:family "FontAwesome" :height 1.2)
                          display
                          (raise -0.24)
                          rear-nonsticky t))
               :replace "\\1"
               :svg-icon ("fa-brands/reddit.svg" :scale 0.72))
             t)
(add-to-list 'telega-url-shorten-regexps
             `(redd.it
               :regexp "^https?://\\(?:\\w+.\\)?redd.it/\\([^?]+\\)\\(?:\\?.*\\)?$"
               :symbol #("" 0 1
                         (face
                          (:family "FontAwesome" :height 1.2)
                          font-lock-face
                          (:family "FontAwesome" :height 1.2)
                          display
                          (raise -0.24)
                          rear-nonsticky t))
               :replace "\\1"
               :svg-icon ("fa-brands/reddit.svg" :scale 0.72))
             t)
(add-to-list 'telega-url-shorten-regexps
             `(stackoverflow
               :regexp "^https?://\\(?:\\w+.\\)?stackoverflow.com/questions/\\([^?]+\\)\\(?:\\?.*\\)?$"
               :symbol #("" 0 1
                         (face
                          (:family "FontAwesome" :height 1.2)
                          font-lock-face
                          (:family "FontAwesome" :height 1.2)
                          display
                          (raise -0.24)
                          rear-nonsticky t))
               :replace "#\\1"
               :svg-icon ("fa-brands/stack-overflow.svg" :scale 0.72))
             t)
(add-to-list 'telega-url-shorten-regexps
             `(stackexchange
               :regexp "^https?://\\(?:\\w+.\\)?stackexchange.com/questions/\\([^?]+\\)\\(?:\\?.*\\)?$"
               :symbol #("" 0 1
                         (face
                          (:family "FontAwesome" :height 1.2)
                          font-lock-face
                          (:family "FontAwesome" :height 1.2)
                          display
                          (raise -0.24)
                          rear-nonsticky t))
               :replace "#\\1"
               :svg-icon ("fa-brands/stack-exchange.svg" :scale 0.72))
             t)
(add-to-list 'telega-url-shorten-regexps
             `(telegram
               :regexp "^https?://t.me/\\(.+\\)$"
               :symbol #("" 0 1
                         (face
                          (:family "FontAwesome" :height 1.2)
                          font-lock-face
                          (:family "FontAwesome" :height 1.2)
                          display
                          (raise -0.24)
                          rear-nonsticky t))
               :replace "tg:\\1"
               :svg-icon ("fa-brands/telegram.svg" :scale 0.72))
             t)
(add-to-list 'telega-url-shorten-regexps
             `(emacs-china
               :regexp "^https?://\\(?:www.\\)?emacs-china.org/\\([^?]+\\)\\(?:\\?.*\\)?$"
               :symbol #("" 0 1
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
             `(too-long-link
               :regexp "^https?://\\(?:www.\\)?\\(.\\{60\\}\\).*?$"
               :symbol #("" 0 1
                         (face
                          (:family "FontAwesome" :height 1.2)
                          font-lock-face
                          (:family "FontAwesome" :height 1.2)
                          display
                          (raise -0.24)
                          rear-nonsticky t))
               :replace "\\1..."
               :svg-icon ("fa-brands/link.svg" :scale 0.72))
             t)
(add-to-list 'telega-url-shorten-regexps
             `(other-link
               :regexp "^https?://\\(?:www.\\)?\\(.+[^/?]\\)[/?]?"
               :symbol #("" 0 1
                         (face
                          (:family "FontAwesome" :height 1.2)
                          font-lock-face
                          (:family "FontAwesome" :height 1.2)
                          display
                          (raise -0.24)
                          rear-nonsticky t))
               :replace "\\1"
               :svg-icon ("fa-brands/link.svg" :scale 0.72))
             t)

(require 'telega-dired-dwim)
(require 'telega-bridge-bot)
