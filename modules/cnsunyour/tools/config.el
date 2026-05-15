;;; cnsunyour/tools/config.el -*- lexical-binding: t; -*-


;; gpg 可以读取在 emacs 中输入的密码
(use-package! pinentry
  :config
  (pinentry-start))

;; Search the word at point with Dash
(use-package! dash-at-point
  :defer t
  :init
  (map! :leader
        "dd" #'dash-at-point
        "dD" #'dash-at-point-with-docset))

;; Automatically save file content
(use-package! super-save
  :custom
  (super-save-delete-trailing-whitespace t)
  (super-save-all-buffers t)
  :config
  (super-save-mode +1))

;; neopastebin -- emacs pastebin interface
(use-package! neopastebin
  :defer t
  :commands (pastebin-list-buffer-refresh
             pastebin-new)
  :config
  (when (modulep! :editor evil +everywhere)
    (map! :map pastebin--list-map
          :n "d" #'pastebin-delete-paste-at-point
          :n "r" #'pastebin-list-buffer-refresh
          :n "F" #'pastebin-list-buffer-refresh-sort-by-format
          :n "T" #'pastebin-list-buffer-refresh-sort-by-title
          :n "K" #'pastebin-list-buffer-refresh-sort-by-key
          :n "D" #'pastebin-list-buffer-refresh-sort-by-date
          :n "P" #'pastebin-list-buffer-refresh-sort-by-private
          :n "q" #'kill-current-buffer))
    (pastebin-create-login :username (auth-source-pick-first-password
                                      :host "pastebin.com"
                                      :user "username")
                           :dev-key (auth-source-pick-first-password
                                      :host "pastebin.com"
                                      :user "dev-key")
                           :password (auth-source-pick-first-password
                                      :host "pastebin.com"
                                      :user "password")))

;; Track Emacs commands frequency
;; (use-package! keyfreq
;;   :config
;;   (keyfreq-mode 1)
;;   (keyfreq-autosave-mode 1))

(use-package! ialign
  :commands ialign
  :bind ("C-S-s-i" . #'ialign))

(use-package! nyan-mode
  :after doom-modeline
  :hook (doom-modeline-mode . nyan-mode)
  :custom
  (nyan-minimum-window-width 100)
  (nyan-animate-nyancat t)
  (nyan-wavy-trail t))

(use-package! blamer
  :defer 20
  :bind
  ("C-x v p" . blamer-show-posframe-commit-info)
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background unspecified
                   :height 140
                   :italic t)))
  :config
  (global-blamer-mode -1))

(use-package! clutch
  :custom
  (clutch-connection-alist
   '(("local-mysql" . (:host "localhost"
                       :port 3306
                       :user "root"
                       :database "mysql"
                       :backend 'mysql))
     ("local-pg" . (:host "localhost"
                    :port 5432
                    :user "postgres"
                    :database "postgres"
                    :backend 'pg)))))
