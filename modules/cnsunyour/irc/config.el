;;; cnsunyour/irc/config.el -*- lexical-binding: t; -*-

(map! "C-M-S-s-r" #'=irc)

(after! all-the-icons
  (add-to-list 'all-the-icons-mode-icon-alist
               '(circe-mode all-the-icons-fileicon "circle-ci"
                            :heigt 1.0
                            :v-adjust -0.2))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(circe-chat-mode all-the-icons-fileicon "circle-ci"
                            :heigt 1.0
                            :v-adjust -0.2)))

(after! circe
  (let* ((authinfo (auth-source-user-and-password "chat.freenode.net"))
         (user (car authinfo))
         (password (cadr authinfo)))
    (set-irc-server! "freenode"
                     `(:use-tls t
                       :host "chat.freenode.net"
                       :port 6697
                       :nick ,user
                       :user ,user
                       :realname "Sunn Yao"
                       :nickserv-nick ,user
                       :nickserv-password ,password
                       :sasl-username ,user
                       :sasl-password ,password
                       :channels ("#archlinux"
                                  "#gentoo"
                                  "#debian"
                                  "##C"
                                  "##c++"
                                  "##java"
                                  "#python"
                                  "#emacs"
                                  "#qutebrowser"
                                  "#weechat")))))
