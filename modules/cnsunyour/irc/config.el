;;; cnsunyour/irc/config.el -*- lexical-binding: t; -*-

(map! "C-M-S-s-r" #'=irc)

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
                       :nickserv-password ,password
                       :sasl-username ,user
                       :sasl-password ,password
                       :channels ("#archlinux"
                                  "##C"
                                  "##c++"
                                  "##C++general"
                                  "#debian"
                                  "#emacs"
                                  "#gentoo"
                                  "#go-nuts"
                                  "##java"
                                  "#linuxba"
                                  "#python"
                                  "#qutebrowser"
                                  "#weechat"
                                  "#wikipedia-zh")))))
