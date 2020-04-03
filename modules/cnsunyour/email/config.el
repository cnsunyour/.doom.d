;;; cnsunyour/email/config.el -*- lexical-binding: t; -*-


;; mu4e
(when (featurep! :email mu4e)
  (setq +mu4e-backend 'offlineimap)

  (set-email-account! "gmail"
                      '((user-full-name         . "凡 (Sunn Yao)")
                        (user-mail-address      . "sunyour@gmail.com")
                        (smtpmail-smtp-user     . "sunyour@gmail.com")
                        (mu4e-sent-folder       . "/gmail/[Gmail].已发邮件")
                        (mu4e-drafts-folder     . "/gmail/[Gmail].草稿")
                        (mu4e-trash-folder      . "/gmail/[Gmail].已删除邮件")
                        (mu4e-refile-folder     . "/gmail/[Gmail].所有邮件")
                        (mu4e-compose-signature . "凡 (Sunn Yao)"))
                      t)

  (set-email-account! "qq"
                      '((user-full-name         . "凡 (Sunn Yao)")
                        (user-mail-address      . "sunyour@qq.com")
                        (smtpmail-smtp-user     . "sunyour@qq.com")
                        (mu4e-sent-folder       . "/qq/Sent Messages")
                        (mu4e-drafts-folder     . "/qq/Drafts")
                        (mu4e-trash-folder      . "/qq/Deleted Messages")
                        (mu4e-refile-folder     . "/qq/其他文件夹.Archive")
                        (mu4e-compose-signature . "凡 (Sunn Yao)")))

  (set-email-account! "163vip"
                      '((user-full-name         . "姚晖 (Sunn Yao)")
                        (user-mail-address      . "sunyour@vip.163.com")
                        (smtpmail-smtp-user     . "sunyour@vip.163.com")
                        (mu4e-sent-folder       . "/163vip/已发送")
                        (mu4e-drafts-folder     . "/163vip/草稿箱")
                        (mu4e-trash-folder      . "/163vip/已删除")
                        (mu4e-refile-folder     . "/163vip/Archive")
                        (mu4e-compose-signature . "姚晖 (Sunn Yao)")))

  (set-email-account! "aliyun"
                      '((user-full-name         . "凡 (Sunn Yao)")
                        (user-mail-address      . "sunyour@aiyun.com")
                        (smtpmail-smtp-user     . "sunyour@aiyun.com")
                        (mu4e-sent-folder       . "/aliyun/已发送")
                        (mu4e-drafts-folder     . "/aliyun/草稿")
                        (mu4e-trash-folder      . "/aliyun/已删除邮件")
                        (mu4e-refile-folder     . "/aliyun/存档")
                        (mu4e-compose-signature . "凡 (Sunn Yao)")))

  (set-email-account! "139"
                      '((user-full-name         . "姚晖 (Sunn Yao)")
                        (user-mail-address      . "sunyour@139.com")
                        (smtpmail-smtp-user     . "sunyour@139.com")
                        (mu4e-sent-folder       . "/139/已发送")
                        (mu4e-drafts-folder     . "/139/草稿箱")
                        (mu4e-trash-folder      . "/139/已删除")
                        (mu4e-refile-folder     . "/139/Archive")
                        (mu4e-compose-signature . "姚晖 (Sunn Yao)")))

  (set-email-account! "dbuav"
                      '((user-full-name         . "姚晖 (Sunn Yao)")
                        (user-mail-address      . "sunyour@dbuav.com")
                        (smtpmail-smtp-user     . "sunyour@dbuav.com")
                        (mu4e-sent-folder       . "/dbuav/已发送")
                        (mu4e-drafts-folder     . "/dbuav/草稿")
                        (mu4e-trash-folder      . "/dbuav/已删除邮件")
                        (mu4e-refile-folder     . "/dbuav/存档")
                        (mu4e-compose-signature . "姚晖 (Sunn Yao)")))

  (map! "C-S-s-m" #'=mu4e))


;; notmuch
(when (featurep! :email notmuch)
  (setq +notmuch-sync-backend 'offlineimap)
  (setq +notmuch-mail-folder "~/.mail")

  (map! "C-S-s-n" #'=notmuch))
