;;; cnsunyour/tools/config.el -*- lexical-binding: t; -*-


;; 判断网络是否连通
(defun internet-up-p (&optional host)
    (= 0 (call-process "ping" nil nil nil "-c" "1" "-W" "1"
                       (if host host "www.google.com"))))

;; gpg 可以读取在 emacs 中输入的密码
(use-package! pinentry :config (pinentry-start))

;; 让flycheck检查载入el文件时从load-path里搜索
(setq flycheck-emacs-lisp-load-path 'inherit)

;; ispell: fix "zh_CN" dict error
(after! ispell
  (ispell-change-dictionary "american" t))

;; docker management
(after! docker
  (setq docker-image-run-arguments '("-i" "-t" "--rm")))

;; Search the word at point with Dash
(use-package! dash-at-point
  :defer t
  :init
  (map! :leader
        "dd" #'dash-at-point
        "dD" #'dash-at-point-with-docset))

;; Automatically save file content
(use-package! auto-save
  :custom
  (auto-save-idle 10 "Increase idle time to auto save files.")
  (auto-save-silent nil "Nothing to dirty minibuffer if this option is non-nil.")
  (auto-save-delete-trailing-whitespace nil "Trailing whitespace when save files.")
  :config
  (auto-save-enable))

;; neopastebin -- emacs pastebin interface
(use-package! neopastebin
  :defer t
  :commands (pastebin-list-buffer-refresh
             pastebin-new)
  :config
  (when (featurep! :editor evil +everywhere)
    (map! :map pastebin--list-map
          :n "d" #'pastebin-delete-paste-at-point
          :n "r" #'pastebin-list-buffer-refresh
          :n "F" #'pastebin-list-buffer-refresh-sort-by-format
          :n "T" #'pastebin-list-buffer-refresh-sort-by-title
          :n "K" #'pastebin-list-buffer-refresh-sort-by-key
          :n "D" #'pastebin-list-buffer-refresh-sort-by-date
          :n "P" #'pastebin-list-buffer-refresh-sort-by-private
          :n "q" #'kill-current-buffer))
  (let ((credentials (auth-source-user-and-password "pastebin")))
    (pastebin-create-login :username "cnsunyour"
                           :dev-key (car credentials)
                           :password (cadr credentials))))

(use-package! keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

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
