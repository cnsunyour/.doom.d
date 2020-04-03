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

(defun cnsunyour/insert-image-from-clipboard ()
  "保存剪切板图片为 Y-m-d-H-M-S.png，插入 Markdown/Org/telega 图片链接."
  (interactive)
  (setq file (format-time-string"%Y-%m-%d-%H-%M-%S.jpg"))
  (cond ((derived-mode-p 'telega-chat-mode)
         (call-process-shell-command (format "pngpaste ~/.telega/temp/%s" file))
         (telega-chatbuf--attach-tmp-photo (format "~/.telega/temp/%s" file)))
        (t (insert file))))
(map! "C-M-S-s-v" #'cnsunyour/insert-image-from-clipboard)

;; Automatically save file content
(use-package! auto-save
  :custom
  (auto-save-idle 10 "Increase idle time to auto save files.")
  (auto-save-silent nil "Nothing to dirty minibuffer if this option is non-nil.")
  (auto-save-delete-trailing-whitespace t "Trailing whitespace when save files.")
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

;; fuz.el，目前snails在用
(use-package! fuz
  :config
  (unless (require 'fuz-core nil t)
    (fuz-build-and-load-dymod)))

;; A modern, easy-to-expand fuzzy search framework
;; M-x snails or M-x snails-search-point
(use-package! snails
  :defer t
  :commands (snails
             snails-search-point)
  :custom
  (snails-use-exec-path-from-shell nil)
  :init
  (map! :leader
        "sn" #'snails
        "sN" #'snails-search-point)
  :config
  (set-evil-initial-state! 'snails-mode 'emacs))
