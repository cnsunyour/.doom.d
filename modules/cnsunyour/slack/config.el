;;; cnsunyour/slack/config.el -*- lexical-binding: t; -*-


(use-package! slack
  :commands (slack-start)
  :bind
  ("C-c s s" . slack-start)
  :init
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  :config
  (slack-register-team
   :name "pterdev"
   :default t
   :token (auth-info-password (car (auth-source-search
                                    :host "my.slack.com"
                                    :user "sunyour@gmail.com")))
   :cookie (auth-info-password (car (auth-source-search
                                     :host "my.slack.com"
                                     :user "sunyour@gmail.com^cookie")))
   :subscribed-channels '(admin
                          announcements
                          cicd
                          general
                          git-alert
                          pterclub-dev
                          random
                          release
                          service-alert
                          tracker-dev)
   :full-and-display-names t)

  ;; (set-evil-initial-state! '(slack-mode) 'emacs)

  (set-popup-rule! "^\\*Slack"
   :side 'right :size .5 :select t :ttl nil :quit t :modeline t)

  (map!
   "C-c s u" #'slack-select-unread-rooms
   "C-c s r" #'slack-select-rooms
   "C-c s t" #'slack-all-threads
   "C-c s i" #'slack-im-select
   "C-c s c" #'slack-channel-select
   "C-c s g" #'slack-group-select)
  (evil-define-key 'normal slack-info-mode-map
    ",u" 'slack-room-update-messages)
  (evil-define-key 'normal slack-mode-map
    ",c" 'slack-buffer-kill
    ",ra" 'slack-message-add-reaction
    ",rr" 'slack-message-remove-reaction
    ",rs" 'slack-message-show-reaction-users
    ",pl" 'slack-room-pins-list
    ",pa" 'slack-message-pins-add
    ",pr" 'slack-message-pins-remove
    ",mm" 'slack-message-write-another-buffer
    ",me" 'slack-message-edit
    ",md" 'slack-message-delete
    ",u" 'slack-room-update-messages
    ",2" 'slack-message-embed-mention
    ",3" 'slack-message-embed-channel
    "\C-n" 'slack-buffer-goto-next-message
    "\C-p" 'slack-buffer-goto-prev-message)
  (evil-define-key 'normal slack-edit-message-mode-map
    ",k" 'slack-message-cancel-edit
    ",s" 'slack-message-send-from-buffer
    ",2" 'slack-message-embed-mention
    ",3" 'slack-message-embed-channel)

  (define-advice slack-room-display (:after (&rest _) toggle-input-method-a)
    "在 slack 里切换输入法"
    (activate-input-method default-input-method)))
