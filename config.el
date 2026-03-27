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
(let* ((brew-prefix (or (getenv "HOMEBREW_PREFIX")
                        (and IS-MAC
                             (cond ((file-directory-p "/opt/homebrew/opt") "/opt/homebrew")
                                   ((file-directory-p "/usr/local/opt") "/usr/local")))
                        "/usr/local"))
       (pkg-config-paths
        (mapcar (lambda (pkg)
                  (expand-file-name (format "opt/%s/lib/pkgconfig" pkg) brew-prefix))
                '("libffi" "qt" "nss" "zlib")))
       (existing-pkg-config-path (getenv "PKG_CONFIG_PATH")))
  (setenv "PKG_CONFIG_PATH"
          (mapconcat #'identity
                     (append pkg-config-paths
                             (unless (or (null existing-pkg-config-path)
                                         (string= existing-pkg-config-path ""))
                               (list existing-pkg-config-path)))
                     path-separator)))

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
  (add-hook! 'php-mode-hook
    (add-hook 'before-save-hook #'php-cs-fixer-before-save nil t))
  (add-hook! 'php-ts-mode-hook
    (add-hook 'before-save-hook #'php-cs-fixer-before-save nil t))
  (after! php-mode
    (setq lsp-intelephense-licence-key
          (auth-source-pick-first-password
           :host "intelephense"))))

(after! magit
  (require 'forge))

(when (modulep! :editor evil)
  (setq evil-magic 'very-magic)
  (setq evil-ex-search-vim-style-regexp t)
  (evil-select-search-module 'evil-search-module 'evil-search))

(define-advice bookmark-jump (:after (bookmark &rest _) reorder-bookmark-a)
  (let ((latest (bookmark-get-bookmark bookmark)))
    (setq bookmark-alist (delq latest bookmark-alist))
    (add-to-list 'bookmark-alist latest)))

(load (expand-file-name ".private.el" doom-user-dir) t)

;; Doom 只在 custom-file 未被修改时自动加载，由于 init.el 修改了路径，需要显式加载
(load (expand-file-name ".custom.el" doom-user-dir) t)
