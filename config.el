;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
(setq user-full-name "Sunn Yao"
      frame-title-format (concat "%b - " user-full-name "'s Emacs")
      user-mail-address "sunyour@gmail.com"
      epa-file-encrypt-to user-mail-address)

;; System locale to use for formatting time values.
;;
;; Make sure that the weekdays in the time stamps of your Org mode files and in
;; the agenda appear in English.
(setq system-time-locale "C")

;; 设置我所在地方的经纬度，calendar里有个功能是日月食的预测，和经纬度相联系的。
;; 让emacs能计算日出日落的时间，在 calendar 上用 S 即可看到
;; 另外根据日出日落时间切换主题也需要经纬度
(setq calendar-location-name "Beijing, China")
(setq calendar-latitude +39.9055472)
(setq calendar-longitude +116.3887056)

;; init ccls include path
(after! ccls
  (when IS-MAC
    (setq ccls-initialization-options
          `(:clang ,(list :extraArgs ["-isystem/Library/Developer/CommandLineTools/usr/include/c++/v1"
                                      "-isystem/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include"
                                      "-isystem/usr/local/include"]
                          :resourceDir (string-trim (shell-command-to-string "clang -print-resource-dir")))))))

;; define environmental variable for some works
(setenv "PKG_CONFIG_PATH"
        (replace-regexp-in-string
         (concat path-separator "+$")
         ""
         (concat
          "/usr/local/opt/libffi/lib/pkgconfig" path-separator
          "/usr/local/opt/qt/lib/pkgconfig" path-separator
          "/usr/local/opt/nss/lib/pkgconfig" path-separator
          "usr/local/opt/zlib/lib/pkgconfig" path-separator
          (getenv "PKG_CONFIG_PATH"))))

;; 使用系统废纸篓删除文件
;; (setq delete-by-moving-to-trash t)
;; (when (eq window-system 'mac)
;;   (setq mac-system-move-file-to-trash-use-finder t))

;; Set default directory
(add-hook! emacs-startup (setq default-directory "~"))

;;
;; Optimize garbage-collect
;; Related variable: `gc-cons-threshold'
;; (setq gc-cons-threshold 2147483648)
;;
;; Execute `garbage-collect' when emacs is idle for a specified time,
;; and count the time consumed.
;; (defmacro k-time (&rest body)
;;   "Measure and return the time it takes evaluating BODY."
;;   `(let ((time (current-time)))
;;      ,@body
;;      (float-time (time-since time))))
;; (run-with-idle-timer 300 t
;;                      (lambda ()
;;                        (message "Garbage Collector has run for %.06fsec"
;;                                 (k-time (garbage-collect)))))
;;
;; Execute `garbage-collect' when emacs is idle for a specified time.
;; (run-with-idle-timer 300 t #'garbage-collect)

(setenv "SSH_AUTH_SOCK" "/Users/yaohui/.gnupg/S.gpg-agent.ssh")

(setq which-key-idle-delay 1.0
      which-key-idle-secondary-delay 0.0)

(after! warnings
  (when (modulep! :emacs undo)
    (add-to-list 'warning-suppress-types '(undo discard-info))))

(after! so-long
  (setq so-long-threshold 1000))

(when (modulep! :app rss)
  (add-hook! 'elfeed-search-mode-hook 'elfeed-update))

(when (modulep! :lang php +lsp)
  (add-hook! before-save #'php-cs-fixer-before-save)
  (after! php-mode
    (setq lsp-intelephense-licence-key
          (auth-source-pick-first-password
           :host "intelephense"))))

(when (modulep! :tools lsp)
  (setq lsp-intelephense-multi-root nil
        lsp-pyright-multi-root nil
        lsp-solargraph-multi-root nil))

(after! magit
  (require 'forge)
  (setq magit-revision-show-gravatars t))

(when (modulep! :editor evil)
  (setq evil-magic 'very-magic)
  (setq evil-ex-search-vim-style-regexp t)
  (evil-select-search-module 'evil-search-module 'evil-search))

(defadvice bookmark-jump (after bookmark-jump activate)
  (let ((latest (bookmark-get-bookmark bookmark)))
    (setq bookmark-alist (delq latest bookmark-alist))
    (add-to-list 'bookmark-alist latest)))

(load (expand-file-name ".private.el" doom-user-dir) t)
