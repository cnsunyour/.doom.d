;;; cnsunyour/tools/config.el -*- lexical-binding: t; -*-


;; 判断网络是否连通
(defun internet-up-p (&optional host)
    (= 0 (call-process "ping" nil nil nil "-c" "1" "-W" "1"
                       (if host host "www.google.com"))))

;; 启用epa-file，可以解密.authinfo.gpg文件
;; (use-package! epa-file :config (epa-file-enable))
;; 启用auth-source-pass，可以使用.password-store里的密码
;; (use-package! auth-source-pass :config (auth-source-pass-enable))
;; 启用auto-soure，读取.autoinfo或.authinfo.gpg里的难信息
;; (use-package! auth-source)

;; gpg 可以读取在 emacs 中输入的密码
(use-package! pinentry :config (pinentry-start))

;; Symbol Overlay 多关键字高亮插件
;; Highlight symbols with overlays while providing a keymap for various
;; operations about highlighted symbols. It was originally inspired by
;; the package highlight-symbol. The fundamental difference is that in
;; symbol-overlay every symbol is highlighted by the Emacs built-in
;; function overlay-put rather than the font-lock mechanism used in
;; highlight-symbol.
;; Default key-bindings defined in symbol-overlay-map:
;; "i" -> symbol-overlay-put
;; "n" -> symbol-overlay-jump-next
;; "p" -> symbol-overlay-jump-prev
;; "w" -> symbol-overlay-save-symbol
;; "t" -> symbol-overlay-toggle-in-scope
;; "e" -> symbol-overlay-echo-mark
;; "d" -> symbol-overlay-jump-to-definition
;; "s" -> symbol-overlay-isearch-literally
;; "q" -> symbol-overlay-query-replace
;; "r" -> symbol-overlay-rename
(map! :g "M-i" 'symbol-overlay-put
      :g "M-n" 'symbol-overlay-switch-forward
      :g "M-p" 'symbol-overlay-switch-backward
      :g "<f7>" 'symbol-overlay-mode
      :g "<f8>" 'symbol-overlay-remove-all)

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
        :g "dd" #'dash-at-point
        :g "dD" #'dash-at-point-with-docset))

(defun cnsunyour/insert-image-from-clipboard ()
  "保存剪切板图片为 Y-m-d-H-M-S.png，插入 Markdown/Org/telega 图片链接."
  (interactive)
  (setq file (format-time-string"%Y-%m-%d-%H-%M-%S.jpg"))
  (cond ((derived-mode-p 'telega-chat-mode)
         (call-process-shell-command (format "pngpaste ~/.telega/temp/%s" file))
         (telega-chatbuf--attach-tmp-photo (format "~/.telega/temp/%s" file)))
        (t (insert file))))
(map! :g "C-M-S-s-v" #'cnsunyour/insert-image-from-clipboard)

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
  :commands
  pastebin-list-buffer-refresh
  pastebin-new
  :config
  (when (featurep! :editor evil +everywhere)
    (evil-define-key 'normal pastebin--list-map
      "d" #'pastebin-delete-paste-at-point
      "r" #'pastebin-list-buffer-refresh
      "F" #'pastebin-list-buffer-refresh-sort-by-format
      "T" #'pastebin-list-buffer-refresh-sort-by-title
      "K" #'pastebin-list-buffer-refresh-sort-by-key
      "D" #'pastebin-list-buffer-refresh-sort-by-date
      "P" #'pastebin-list-buffer-refresh-sort-by-private
      "q" #'kill-current-buffer
      (kbd "ESC ESC ESC") #'kill-current-buffer))
  (let ((credentials (auth-source-user-and-password "pastebin")))
    (pastebin-create-login :username "cnsunyour"
                           :dev-key (car credentials)
                           :password (cadr credentials))))

;; Ensure environment variables inside Emacs look the same as in the user's shell.
;; Snails needed currently.
(use-package! exec-path-from-shell
  :when IS-MAC
  :config
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

;; fuz.el，目前snails在用
(use-package! fuz
  :config
  (unless (require 'fuz-core nil t)
    (fuz-build-and-load-dymod)))

;; A modern, easy-to-expand fuzzy search framework
;; M-x snails or M-x snails-search-point
(use-package! snails
  :defer t
  :commands snails snails-search-point
  :init
  (map! :leader
        (:g "sn" #'snails)
        (:g "sN" #'snails-search-point))
  :config
  (set-evil-initial-state! 'snails-mode 'emacs))
