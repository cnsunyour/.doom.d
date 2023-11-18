;;; cnsunyour/telega/config.el -*- lexical-binding: t; -*-

;; telegram client for emacs
(use-package! telega
  :defer t

  :init
  (define-key global-map (kbd "C-c t") telega-prefix-map)
  (unless (display-graphic-p) (setq telega-use-images nil))
  (when (modulep! :editor evil)
    (cl-pushnew 'telega +evil-collection-disabled-list :test #'equal)
    (setq evil-collection-mode-list (remove 'telega evil-collection-mode-list))
    (set-evil-initial-state! '(telega-root-mode telega-chat-mode) 'emacs))

  :hook
  (telega-load . telega-mode-line-mode)
  (telega-load . global-telega-url-shorten-mode)
  (telega-load . global-telega-mnz-mode)
  (telega-load . telega-autoplay-mode)
  (telega-load . telega-transient-mode)

  :config
  ;; (add-hook 'telega-msg-ignore-predicates
  ;;           (telega-match-gen-predicate "msg-" '(sender blocked)))
  (setq telega-chat-show-deleted-messages-for '(me-is-owner OR-ADMIN)
        ;; telega-use-tracking-for '(or mention (and unread unmuted))
        telega-open-file-function 'org-open-file
        ;; telega-open-message-as-file '(video video-note)
        telega-sticker-size '(8 . 48)
        telega-translate-to-language-by-default "zh"
        telega-avatar-workaround-gaps-for `(return t)
        telega-mode-line-string-format (remove
                                        '(:eval (telega-mode-line-icon))
                                        telega-mode-line-string-format))

  (map! :desc "Telega all chats"
        "C-c c" #'telega-chat-with
        :desc "Telega important chats"
        "C-c v" #'telega-switch-important-chat
        :desc "Telega next important chat"
        "C-c SPC" (cmd! (let ((current-prefix-arg '(4)))
                          (call-interactively #'telega-switch-important-chat))))
  (map! :map telega-chat-mode-map
        "C-c C-e" #'telega-chatbuf-attach-sticker
        "C-c C-t" #'telega-auto-translate-mode)

  (load! "+telega-auto-input-method")

  (set-popup-rule! (regexp-quote telega-root-buffer-name)
    :slot 10 :vslot 10 :side 'right :size (+ telega-chat-fill-column 11) :ttl nil :quit 'current :modeline t)
  (set-popup-rule! "^◀[^◀\[]*[\[({<].+[\])}>]"
    :slot 10 :vslot 10 :side 'right :size (+ telega-chat-fill-column 11) :ttl 10 :quit 'current :modeline t)
  (set-popup-rule! (regexp-quote "*Telega Instant View*")
    :slot 20 :vslot 10 :side 'right :height .5 :ttl 10 :quit t :modeline nil :select t)
  (set-popup-rule! (regexp-quote "*Telega User*")
    :slot 20 :vslot 10 :side 'right :height .5 :ttl 10 :quit t :modeline nil :select t)
  (set-popup-rule! (regexp-quote "*Telegram Chat Info*")
    :slot 20 :vslot 10 :side 'right :height .5 :ttl 10 :quit t :modeline nil :select t)

  (load! "+telega-addition"))


(use-package! telega-bridge-bot :after telega)
