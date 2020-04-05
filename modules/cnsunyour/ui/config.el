;;; cnsunyour/ui/config.el -*- lexical-binding: t; -*-

(defun cnsunyour/set-doom-font ()
  "Set random font family and size."
  (let* ((fonts (cl-remove-if
                 (lambda (elf)
                   (not (member elf '("Sarasa Mono SC"
                                      "Sarasa Mono Slab SC"
                                      "等距更纱黑体 SC"
                                      "等距更纱黑体 Slab SC"
                                      ;; "PragmataPro"
                                      ;; "Iosevka"
                                      ;; "Inconsolata"
                                      "Unifont"
                                      "Noto Sans Mono CJK SC"
                                      "WenQuanYi Zen Hei Mono"
                                      "文泉驿等宽正黑"
                                      "M+ 1m"
                                      "M+ 1mn"
                                      "M+ 2m"))))
                 (mapcar (lambda (str)
                           (decode-coding-string str 'utf-8))
                         (cl-remove-duplicates (font-family-list)))))
         (font (elt fonts (random (length fonts))))
         (font-size (if (and (>= (x-display-pixel-width) 1600)
                             (>= (x-display-pixel-height) 1000))
                        16 14)))
    (when font
      (setq doom-font (font-spec :family font :size font-size)))))
      ;; (setq doom-unicode-font
      ;;       (if IS-MAC
      ;;           (font-spec :family "Apple Color Emoji"
      ;;                      :size (if large-display-p
      ;;                                (* large-font-size .8)
      ;;                              (* small-font-size .8)))
      ;;         (font-spec :family (if (member
      ;;                                 "Noto Color Emoji"
      ;;                                 (font-family-list))
      ;;                                "Noto Color Emoji"
      ;;                              "Symbola")))))))
      ;; (doom/reload-font)
;; Set default font when theme changed.
;; (add-hook 'doom-load-theme-hook #'cnsunyour/set-doom-font)
;; Or, you can set font manually now.
(cnsunyour/set-doom-font)

(defun cnsunyour/set-splash-image ()
  "Set random splash image."
  (setq fancy-splash-image
        (let* ((banners (directory-files "~/.doom.d/banner" 'full (rx ".png" eos)))
               (banner (elt banners (random (length banners)))))
          banner)))
;; Set splash image when theme changed.
(add-hook 'doom-load-theme-hook #'cnsunyour/set-splash-image)

;; 设定popup的窗口形式为右侧开启，宽度为40%
;; (set-popup-rule! "^\\*" :side 'right :size 0.5 :select t)

;; 80列太窄，120列太宽，看着都不舒服，100列正合适
;; (setq-default fill-column 100)

;; 虚拟换行设置
;; (setq-default visual-fill-column-width 120)
;; (global-visual-fill-column-mode 1)
;; (global-visual-line-mode 1)

;; To fix the issue: Unable to load color "brightblack"
(after! hl-fill-column
  (set-face-background 'hl-fill-column-face "#555555"))

(after! doom-modeline
  (setq doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-state-icon t
        doom-modeline-buffer-modification-icon t
        doom-modeline-enable-word-count t
        doom-modeline-indent-info t))

(after! lsp-ui
  (setq lsp-ui-doc-position 'at-point
        lsp-ui-flycheck-enable t
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-update-mode 'point
        lsp-enable-file-watchers nil
        lsp-ui-doc-enable t)
  (if (featurep 'xwidget-internal)
      (setq lsp-ui-doc-use-webkit t)))

;; winum，使用SPC+[0-9]选择窗口
(after! winum
  (map! :leader
        "0" #'winum-select-window-0-or-10
        "1" #'winum-select-window-1
        "2" #'winum-select-window-2
        "3" #'winum-select-window-3
        "4" #'winum-select-window-4
        "5" #'winum-select-window-5
        "6" #'winum-select-window-6
        "7" #'winum-select-window-7
        "8" #'winum-select-window-8
        "9" #'winum-select-window-9))

;; 拆分窗口时默认把焦点定在新窗口，doom为了和vim保持一致，竟然把这点改回去了
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; 使用相对行号
(setq display-line-numbers-type 'relative)

;; disable display line numbers on text-mode
;; (remove-hook 'text-mode-hook #'display-line-numbers-mode)

;; Enabling Font Ligatures in emacs-mac-port
(when (eq window-system 'mac)
  (mac-auto-operator-composition-mode))

;; 调整Mac下窗口和全屏显示方式
(when (eq window-system 'ns)
  (setq ns-use-thin-smoothing t)
  (setq ns-use-native-fullscreen nil)
  (setq ns-use-fullscreen-animation nil))

;; 调整启动时窗口大小/最大化/全屏
;; (pushnew! initial-frame-alist '(width . 200) '(height . 48))
(add-hook 'window-setup-hook #'toggle-frame-maximized t)
;; (add-hook 'window-setup-hook #'toggle-frame-fullscreen t)

;; 每天根据日出日落时间自动换主题
;; 本插件的加载时机很关键，Doom 的加载顺序为：
;;
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
;;
;; 只有放在module config.el files之后，doom-init-ui-hook之前才能正常执行
(use-package! theme-changer
  :custom
  (theme-changer-delay-seconds 1500 "Delay 25 minutes for sync with macOS's auto theme changer.")
  :hook
  ('emacs-startup . (lambda ()
                      (change-theme '(doom-one-light
                                      doom-acario-light
                                      doom-nord-light
                                      doom-opera-light
                                      doom-solarized-light
                                      doom-tomorrow-day
                                      flucui-light
                                      lab-light)
                                    '(doom-one
                                      doom-vibrant
                                      doom-city-lights
                                      doom-challenger-deep
                                      doom-dark+
                                      doom-dracula
                                      doom-gruvbox
                                      doom-Iosvkem
                                      doom-molokai
                                      doom-moonlight
                                      doom-oceanic-next
                                      doom-peacock
                                      doom-snazzy
                                      doom-spacegrey
                                      srcery
                                      flucui-dark
                                      lab-dark)))))

(use-package! awesome-tab
  :commands (awesome-tab-mode)
  :init
  (defhydra hydra-tab (:pre (awesome-tab-mode t)
                       :post (awesome-tab-mode -1))
    "
   ^^^^Fast Move             ^^^^Tab                    ^^Search            ^^Misc
  -^^^^--------------------+-^^^^---------------------+-^^----------------+-^^---------------------------
     ^_k_^   prev group    | _C-a_^^     select first | _b_ switch buffer | _C-k_   kill buffer
   _h_   _l_ switch tab    | _C-e_^^     select last  | _g_ switch group  | _C-S-k_ kill others in group
     ^_j_^   next group    | _a_^^       ace jump     | ^^                | ^^
   ^^0 ~ 9^^ select window | _C-h_/_C-l_ move current | ^^                | ^^
  -^^^^--------------------+-^^^^---------------------+-^^----------------+-^^---------------------------
  "
    ("h" awesome-tab-backward-tab)
    ("j" awesome-tab-forward-group)
    ("k" awesome-tab-backward-group)
    ("l" awesome-tab-forward-tab)
    ("a" awesome-tab-ace-jump)
    ("C-a" awesome-tab-select-beg-tab)
    ("C-e" awesome-tab-select-end-tab)
    ("C-h" awesome-tab-move-current-tab-to-left)
    ("C-l" awesome-tab-move-current-tab-to-right)
    ("b" ivy-switch-buffer)
    ("g" awesome-tab-counsel-switch-group)
    ("C-k" kill-current-buffer)
    ("C-S-k" awesome-tab-kill-other-buffers-in-current-group)
    ("q" nil "quit"))
  :bind
  (("s-t" . hydra-tab/body)))

(use-package! emojify
  :custom
  (emojify-point-entered-behaviour 'uncover)
  :hook
  ('telega-chat-mode . #'emojify-mode))
