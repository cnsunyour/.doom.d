;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
(setq epa-file-encrypt-to user-mail-address)

;; System locale to use for formatting time values.
;;
;; Make sure that the weekdays in the time stamps of your Org mode files and in
;; the agenda appear in English.
(setq system-time-locale "C")

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

(setenv "SSH_AUTH_SOCK" "/Users/yaohui/.gnupg/S.gpg-agent.ssh")

(setq which-key-idle-delay 1.0
      which-key-idle-secondary-delay 0.0)

(after! warnings
  (when (modulep! :emacs undo)
    (add-to-list 'warning-suppress-types '(undo discard-info))))

(when (modulep! :app rss)
  (add-hook! 'elfeed-search-mode-hook 'elfeed-update))

(when (modulep! :lang php +lsp)
  (add-hook! before-save #'php-cs-fixer-before-save)
  (after! php-mode
    (setq lsp-intelephense-licence-key
          (auth-source-pick-first-password
           :host "intelephense"))))

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

(use-package! popwin
  :bind-keymap
  ("C-c q" . popwin:keymap)
  :config
  (popwin-mode 1))

(load (expand-file-name ".private.el" doom-user-dir) t)
