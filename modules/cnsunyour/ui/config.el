;;; cnsunyour/ui/config.el -*- lexical-binding: t; -*-

(use-cjk-char-width-table 'zh_CN)

(when (display-graphic-p)
  (defcustom my-ui-fonts '("Sarasa Mono SC"
                           "Sarasa Mono Slab SC"
                           "WenQuanYi Micro Hei Mono"
                           "WenQuanYi Zen Hei Mono"
                           "LXGW WenKai Mono"
                           "Unifont"
                           "Noto Sans Mono CJK SC")
    "Font lists used in my private custom ui config."
    :group 'my-ui
    :type '(list string))
  (defcustom my-ui-font-zh nil
    "Chinese font used in my private custom ui config."
    :group 'my-ui
    :type 'string)
  (defcustom my-ui-fonts-symbol '("Segoe UI Symbol"
                                  "Apple Symbols"
                                  "Noto Sans Symbols 2"
                                  "Symbola")
    "Symbol fonts used in my private custom ui config."
    :group 'my-ui
    :type 'list)
  (defcustom my-ui-fonts-emoji '("Apple Color Emoji"
                                 "Segoe UI Emoji"
                                 "Twitter Color Emoji"
                                 "Noto Color Emoji"
                                 "Noto Emoji")
    "Emoji fonts used in my private custom ui config."
    :group 'my-ui
    :type 'list)
  (defcustom my-ui-fonts-unicode '("Latin Modern Math"
                                   "Cambria Math"
                                   "Noto Sans Math"
                                   "Noto Sans Egyptian Hieroglyphs"
                                   "Noto Sans EgyptHiero")
    "All other unicode fonts used in my private custom ui config."
    :group 'my-ui
    :type 'list)

  (defvar fontset-scripts-zh '(han kana hangul cjk-misc bopomofo)
    "ZH fontset script name list for `set-fontset-font'")
  (defvar fontset-scripts-symbol '(unicode symbol emoji)
    "symbol/emoji/unicode script name list for `set-fontset-font'")

  (dolist (symbol my-ui-fonts-symbol)
    (add-to-list 'doom-symbol-fallback-font-families symbol t))
  (dolist (emoji my-ui-fonts-emoji)
    (add-to-list 'doom-emoji-fallback-font-families emoji t))

  (when-let* ((font (if (and my-ui-fonts (listp my-ui-fonts))
                        (elt my-ui-fonts (random (length my-ui-fonts)))
                      my-ui-fonts))
              (font-chinese (if my-ui-font-zh
                                my-ui-font-zh
                              font))
              (font-size (if (and (>= (x-display-pixel-width) 1600)
                                  (>= (x-display-pixel-height) 1000))
                             18 16)))
    (setq doom-font (font-spec :family font :size font-size))
    (when (fboundp 'set-fontset-font)
      (add-hook! 'after-setting-font-hook :append
        (dolist (script fontset-scripts-symbol)
          (dolist (font-emoji doom-emoji-fallback-font-families)
            (set-fontset-font t script font-emoji nil 'append))
          (dolist (font-symbol doom-symbol-fallback-font-families)
            (set-fontset-font t script font-symbol nil 'append)))

        (dolist (font-unicode my-ui-fonts-unicode)
          (set-fontset-font t 'unicode font-unicode nil 'append))

        (dolist (script fontset-scripts-zh)
          (set-fontset-font t script font-chinese nil 'prepend)))))

  (when (and my-ui-font-zh
             (fboundp 'doom-adjust-font-size)
             (fboundp 'set-fontset-font))
    (define-advice doom-adjust-font-size (:after (&rest _) reset-chinese-font)
      (dolist (script fontset-scripts-zh)
        (set-fontset-font t script my-ui-font-zh nil 'prepend))))

  ;; (add-hook! 'vterm-mode-hook
  ;;   (setq-local buffer-face-mode-face '((:family "Iosevka Nerd Font")))
  ;;   (buffer-face-mode))

  (add-hook! 'doom-load-theme-hook
    (setq fancy-splash-image
          (let ((banners (directory-files (expand-file-name "banner" doom-user-dir)
                                          'full
                                          (rx ".png" eos))))
            (elt banners (random (length banners))))))
  ) ;; when (display-graphic-p)

;;
;; 每天根据日出日落时间(非macOS)或跟随macOS系统自动换主题
;; 插件的加载时机很关键，Doom 的加载顺序为：
;; ~/.emacs.d/init.el
;; ~/.emacs.d/core/core.el
;; ~/.doom.d/init.el
;; Module init.el files
;; `doom-before-init-modules-hook'
;; Module config.el files
;; ~/.doom.d/config.el
;; `doom-init-modules-hook'
;; `after-init-hook'
;; `emacs-startup-hook'
;; `doom-init-ui-hook'
;; `window-setup-hook'
;; 只有放在module config.el files之后，doom-init-ui-hook之前才能正常执行
;;
(defconst +list-light-theme '(doom-earl-grey
                              doom-feather-light
                              doom-flatwhite
                              doom-nord-light
                              doom-one-light
                              doom-opera-light
                              doom-tomorrow-day))
(defconst +list-dark-theme  '(doom-ayu-mirage
                              doom-challenger-deep
                              doom-city-lights
                              doom-dark+
                              doom-dracula
                              doom-feather-dark
                              doom-gruvbox
                              doom-henna
                              doom-horizon
                              doom-Iosvkem
                              doom-lantern
                              doom-laserwave
                              doom-manegarm
                              doom-material
                              doom-molokai
                              doom-monokai-classic
                              doom-monokai-pro
                              doom-moonlight
                              doom-oceanic-next
                              doom-one
                              doom-palenight
                              doom-peacock
                              doom-rouge
                              doom-snazzy
                              doom-spacegrey
                              doom-tokyo-night
                              doom-tomorrow-night
                              doom-vibrant
                              doom-xcode))

;; Auto change theme on non-mac OS
(use-package! theme-changer
  :unless IS-MAC
  :config
  (add-hook! after-init
             :append
             (change-theme +list-light-theme
                           +list-dark-theme)))

;; Change theme sync with macOS
(use-package! auto-dark-emacs
  :when (and IS-MAC (not (boundp 'ns-system-appearance-change-functions)))
  :config
  (setq auto-dark-emacs/light-theme +list-light-theme
        auto-dark-emacs/dark-theme +list-dark-theme
        auto-dark-emacs/polling-interval-seconds 15)
  (add-hook! after-init
             :append
             #'auto-dark-emacs/check-and-set-dark-mode))

(when (and IS-MAC (boundp 'ns-system-appearance-change-functions))
  (defun cnsunyour/change-theme(appearance)
    "Load theme according to the current system appearance."
    (pcase appearance
      ('light
       (mapc #'disable-theme custom-enabled-themes)
       (load-theme (if (listp +list-light-theme)
                       (elt +list-light-theme
                            (random (length +list-light-theme)))
                     +list-light-theme)
                   t))
      ('dark
       (mapc #'disable-theme custom-enabled-themes)
       (load-theme (if (listp +list-dark-theme)
                       (elt +list-dark-theme
                            (random (length +list-dark-theme)))
                     +list-dark-theme)
                   t))))
  (add-hook 'ns-system-appearance-change-functions #'cnsunyour/change-theme))

;; 设定popup的窗口形式为右侧开启，宽度为40%
;; (set-popup-rule! "^\\*" :side 'right :size 0.5 :select t)

;; 80列太窄，120列太宽，看着都不舒服，100列正合适
;; (setq-default fill-column 100)

;; 虚拟换行设置
;; (setq-default visual-fill-column-width 120)
;; (global-visual-fill-column-mode 1)
;; (global-visual-line-mode 1)

;; To fix the issue: Unable to load color "brightblack"
;; (after! hl-fill-column
;;   (set-face-background 'hl-fill-column-face "#555555"))

;; (after! doom-modeline
;;   (setq doom-modeline-icon t
;;         doom-modeline-major-mode-icon t
;;         doom-modeline-major-mode-color-icon t
;;         doom-modeline-buffer-state-icon t
;;         doom-modeline-buffer-modification-icon t
;;         doom-modeline-enable-word-count t
;;         doom-modeline-indent-info t))

;; (after! lsp-ui
;;   (setq lsp-ui-doc-position 'at-point
;;         lsp-ui-flycheck-enable t
;;         lsp-ui-sideline-ignore-duplicate t
;;         lsp-ui-sideline-update-mode 'point
;;         lsp-enable-file-watchers nil
;;         lsp-ui-doc-enable t)
;;   (if (featurep 'xwidget-internal)
;;       (setq lsp-ui-doc-use-webkit t)))

;; 拆分窗口时默认把焦点定在新窗口，doom为了和vim保持一致，竟然把这点改回去了
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; Line numbers are pretty slow all around. The performance boost of
;; disabling them outweighs the utility of always keeping them on.
(setq display-line-numbers-type 'relative)

;; 调整Mac下窗口和全屏显示方式
(when (eq window-system 'ns)
  (setq ns-use-thin-smoothing t
        ns-use-native-fullscreen nil
        ns-use-fullscreen-animation nil))

;; 调整启动时窗口大小/最大化/全屏
;; (pushnew! initial-frame-alist '(width . 200) '(height . 48))
(add-hook 'window-setup-hook #'toggle-frame-maximized t)
;; (add-hook 'window-setup-hook #'toggle-frame-fullscreen t)

(use-package! nerd-icons)
