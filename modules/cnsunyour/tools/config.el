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
(use-package! super-save
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
  (let ((credentials (auth-source-user-and-password "pastebin")))
    (pastebin-create-login :username "cnsunyour"
                           :dev-key (car credentials)
                           :password (cadr credentials))))

;; Track Emacs commands frequency
;; (use-package! keyfreq
;;   :config
;;   (keyfreq-mode 1)
;;   (keyfreq-autosave-mode 1))

;; use magit-status in vterm
(after! vterm
  (add-to-list 'vterm-eval-cmds '("magit-status" magit-status)))

(use-package! ialign
  :commands ialign
  :bind ("C-S-s-i" . #'ialign))

(use-package! nyan-mode
  :after doom-modeline
  :custom
  (nyan-minimum-window-width 100)
  (nyan-animate-nyancat t)
  (nyan-wavy-trail t)
  :hook
  (doom-modeline-mode . nyan-mode))

(use-package! gptel
  :defer t
  :hook
  (gptel-mode . (lambda ()
                  (display-line-numbers-mode -1)
                  (evil-change-state 'emacs)))
  (gptel-post-response . (lambda ()
                           (pcase (buffer-local-value 'major-mode (current-buffer))
                             ('org-mode (org-next-visible-heading 1) (org-end-of-line))
                             ('markdown-mode (markdown-outline-next) (end-of-line)))))
  :config
  (setq gptel-default-mode 'markdown-mode)
  (set-popup-rule! (regexp-quote "*ChatGPT*")
    :side 'left :size 80 :select t :quit 'current)
  (setq gptel-api-key (auth-source-pick-first-password
                       :host "openai.com"
                       :user "chatgpt"))
  (define-advice gptel-curl--get-args
      (:around (orig-func &rest args) add-proxy)
    (let ((res (apply orig-func args)))
      (cons "-xhttp://127.0.0.1:7890" res))))
