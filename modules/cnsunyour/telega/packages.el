;; -*- no-byte-compile: t; -*-
;;; cnsunyour/telega/packages.el

(package! telega :recipe (:files (:defaults "contrib/*.el" "etc" "server" "Makefile")))
(package! telega-bridge-bot
  :recipe (:host nil
           :repo "https://gitee.com/blindingdark/BEmacs.git"
           :files ("vendor/telega-bridge-bot.el")))
(package! language-detection)
;; (package! tracking)
