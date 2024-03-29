;;; cnsunyour/email/config.el -*- lexical-binding: t; -*-


(defun +utf7-encode-imap-region ()
  (interactive)
  (when mark-active
    (let ((string (buffer-substring-no-properties
                   (region-beginning) (region-end))))
      (delete-region (region-beginning) (region-end))
      (insert (utf7-encode string t)))))

(defun +utf7-decode-imap-region ()
  (interactive)
  (when mark-active
    (let ((string (buffer-substring-no-properties
                   (region-beginning) (region-end))))
      (delete-region (region-beginning) (region-end))
      (insert (utf7-decode string t)))))


(use-package! mu4e-alert
  :when (modulep! :email mu4e)
  :after mu4e
  :config
  (setq doom-modeline-mu4e t
        mu4e-alert-interesting-mail-query "flag:unread AND maildir:/INBOX/ AND NOT flag:trash")

  (mu4e-alert-set-default-style 'notifier)
  (mu4e-alert-enable-notifications)
  (mu4e-alert-enable-mode-line-display))

;; mu4e
(when (modulep! :email mu4e)
  (setenv "XAPIAN_CJK_NGRAM" "1")

  (after! mu4e
    (setq +mu4e-backend 'offlineimap
          mu4e-get-mail-command "offlineimap -o -q"
          send-mail-function #'smtpmail-send-it
          message-send-mail-function send-mail-function
          mu4e-update-interval 300
          mu4e-headers-include-related t
          mu4e-headers-skip-duplicates t)

    (setq mu4e-bookmarks
          `(,(make-mu4e-bookmark
              :name  "Unread messages"
              :query "flag:unread AND maildir:/INBOX/ AND NOT flag:trash"
              :key ?u)
            ,(make-mu4e-bookmark
              :name "Today's messages"
              :query "date:today..now AND maildir:/INBOX/ AND NOT flag:trash"
              :key ?b)
            ,(make-mu4e-bookmark
              :name "Last week"
              :query "date:1w..now AND maildir:/INBOX/ AND NOT flag:trash"
              :key ?w)
            ,(make-mu4e-bookmark
              :name "Last month"
              :query "date:1m..now AND maildir:/INBOX/ AND NOT flag:trash"
              :key ?m)
            ,(make-mu4e-bookmark
              :name "Last season"
              :query "date:3m..now AND maildir:/INBOX/ AND NOT flag:trash"
              :key ?s)
            ,(make-mu4e-bookmark
              :name "Last half year"
              :query "date:6m..now AND maildir:/INBOX/ AND NOT flag:trash"
              :key ?h)
            ,(make-mu4e-bookmark
              :name "Last year"
              :query "date:1y..now AND maildir:/INBOX/ AND NOT flag:trash"
              :key ?y)
            ,(make-mu4e-bookmark
              :name "All in inbox"
              :query "maildir:/INBOX/ AND NOT flag:trash"
              :key ?a)
            ,(make-mu4e-bookmark
              :name "Important"
              :query "prio:high AND maildir:/INBOX/ AND NOT flag:trash"
              :key ?i)
            ,(make-mu4e-bookmark
              :name "Mailing lists"
              :query "flag:list AND maildir:/INBOX/ AND NOT flag:trash"
              :key ?l)
            ,(make-mu4e-bookmark
              :name "With attachments"
              :query "flag:attach AND maildir:/INBOX/ AND NOT flag:trash"
              :key ?p)))

    (set-email-account! "icloud"
                        '((user-full-name              . "姚晖 (Sunn Yao)")
                          (user-mail-address           . "sunyour@me.com")
                          (smtpmail-smtp-user          . "sunyour@me.com")
                          (smtpmail-smtp-server        . "smtp.mail.me.com")
                          (smtpmail-smtp-service       . 587)
                          (smtpmail-stream-type        . starttls)
                          (mu4e-sent-messages-behavior . sent)
                          (mu4e-sent-folder            . "/icloud/Sent Messages")
                          (mu4e-drafts-folder          . "/icloud/Drafts")
                          (mu4e-trash-folder           . "/icloud/Deleted Messages")
                          (mu4e-refile-folder          . "/icloud/Archive")
                          (mu4e-compose-signature      . "姚晖 (Sunn Yao)")))

    (set-email-account! "live"
                        '((user-full-name              . "姚晖 (Sunn Yao)")
                          (user-mail-address           . "sunyour@live.cn")
                          (smtpmail-smtp-user          . "sunyour@live.cn")
                          (smtpmail-smtp-server        . "smtp.office365.com")
                          (smtpmail-smtp-service       . 587)
                          (smtpmail-stream-type        . starttls)
                          (mu4e-sent-messages-behavior . sent)
                          (mu4e-sent-folder            . "/live/Sent")
                          (mu4e-drafts-folder          . "/live/Drafts")
                          (mu4e-trash-folder           . "/live/Deleted")
                          (mu4e-refile-folder          . "/live/存档")
                          (mu4e-compose-signature      . "姚晖 (Sunn Yao)")))

    (set-email-account! "dbuav"
                        '((user-full-name              . "姚晖 (Sunn Yao)")
                          (user-mail-address           . "yh@dbuav.com")
                          (smtpmail-smtp-user          . "yh@dbuav.com")
                          (smtpmail-smtp-server        . "smtp.mxhichina.com")
                          (smtpmail-smtp-service       . 465)
                          (smtpmail-stream-type        . ssl)
                          (mu4e-sent-messages-behavior . sent)
                          (mu4e-sent-folder            . "/dbuav/已发送")
                          (mu4e-drafts-folder          . "/dbuav/草稿")
                          (mu4e-trash-folder           . "/dbuav/已删除邮件")
                          (mu4e-refile-folder          . "/dbuav/存档")
                          (mu4e-compose-signature      . "姚晖 (Sunn Yao)")))

    (set-email-account! "aliyun"
                        '((user-full-name              . "凡 (Sunn Yao)")
                          (user-mail-address           . "sunyour@aiyun.com")
                          (smtpmail-smtp-user          . "sunyour@aiyun.com")
                          (smtpmail-smtp-server        . "smtp.aliyun.com")
                          (smtpmail-smtp-service       . 465)
                          (smtpmail-stream-type        . ssl)
                          (mu4e-sent-messages-behavior . sent)
                          (mu4e-sent-folder            . "/aliyun/已发送")
                          (mu4e-drafts-folder          . "/aliyun/草稿")
                          (mu4e-trash-folder           . "/aliyun/已删除邮件")
                          (mu4e-refile-folder          . "/aliyun/存档")
                          (mu4e-compose-signature      . "凡 (Sunn Yao)")))

    (set-email-account! "163vip"
                        '((user-full-name              . "姚晖 (Sunn Yao)")
                          (user-mail-address           . "sunyour@vip.163.com")
                          (smtpmail-smtp-user          . "sunyour@vip.163.com")
                          (smtpmail-smtp-server        . "smtp.vip.163.com")
                          (smtpmail-smtp-service       . 465)
                          (smtpmail-stream-type        . ssl)
                          (mu4e-sent-messages-behavior . sent)
                          (mu4e-sent-folder            . "/163vip/已发送")
                          (mu4e-drafts-folder          . "/163vip/草稿箱")
                          (mu4e-trash-folder           . "/163vip/已删除")
                          (mu4e-refile-folder          . "/163vip/Archive")
                          (mu4e-compose-signature      . "姚晖 (Sunn Yao)")))

    (set-email-account! "qq"
                        '((user-full-name              . "凡 (Sunn Yao)")
                          (user-mail-address           . "sunyour@qq.com")
                          (smtpmail-smtp-user          . "sunyour@qq.com")
                          (smtpmail-smtp-server        . "smtp.qq.com")
                          (smtpmail-smtp-service       . 587)
                          (smtpmail-stream-type        . starttls)
                          (mu4e-sent-messages-behavior . delete)
                          (mu4e-sent-folder            . "/qq/Sent Messages")
                          (mu4e-drafts-folder          . "/qq/Drafts")
                          (mu4e-trash-folder           . "/qq/Deleted Messages")
                          (mu4e-refile-folder          . "/qq/其他文件夹.存档")
                          (mu4e-compose-signature      . "凡 (Sunn Yao)")))

    (set-email-account! "gmail"
                        '((user-full-name              . "凡 (Sunn Yao)")
                          (user-mail-address           . "sunyour@gmail.com")
                          (smtpmail-smtp-user          . "sunyour@gmail.com")
                          (smtpmail-smtp-server        . "smtp.gmail.com")
                          (smtpmail-smtp-service       . 587)
                          (smtpmail-stream-type        . starttls)
                          (mu4e-sent-messages-behavior . delete)
                          (mu4e-sent-folder            . "/gmail/[Gmail].已发邮件")
                          (mu4e-drafts-folder          . "/gmail/[Gmail].草稿")
                          (mu4e-trash-folder           . "/gmail/[Gmail].已删除邮件")
                          (mu4e-refile-folder          . "/gmail/[Gmail].所有邮件")
                          (mu4e-compose-signature      . "凡 (Sunn Yao)"))
                        t)))


;; notmuch
(when (modulep! :email notmuch)
  (after! notmuch
    (setq +notmuch-sync-backend 'offlineimap)
    (setq +notmuch-mail-folder "~/.mail")))
