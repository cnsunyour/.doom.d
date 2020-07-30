;;; cnsunyour/ui/config.el -*- lexical-binding: t; -*-

(use-cjk-char-width-table 'zh_CN)

(when (display-graphic-p)
  (defun cnsunyour/set-doom-font ()
    "Set random font family and size."
    (let* ((fonts (cl-remove-if
                   (lambda (elf)
                     (not (member elf '("Sarasa Mono SC"
                                        "Sarasa Mono Slab SC"
                                        "等距更纱黑体 SC"
                                        "等距更纱黑体 Slab SC"
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
        ;; (doom/reload-font))))
  ;; Set default font when theme changed.
  ;; (add-hook 'doom-load-theme-hook #'cnsunyour/set-doom-font)
  ;; Or, you can set it manually now.
  (cnsunyour/set-doom-font)

  (defun cnsunyour/set-splash-image ()
    "Set random splash image."
    (setq fancy-splash-image
          (let ((banners (directory-files "~/.doom.d/banner"
                                          'full
                                          (rx ".png" eos))))
            (elt banners (random (length banners))))))
  ;; Set splash image when theme changed.
  (add-hook 'doom-load-theme-hook #'cnsunyour/set-splash-image))
  ;; Or, you can set it manually now.
  ;; (cnsunyour/set-splash-image)


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
(setq +list-light-theme '(doom-one-light
                                    doom-acario-light
                                    doom-nord-light
                                    doom-opera-light
                                    doom-solarized-light
                                    doom-tomorrow-day
                                    flucui-light
                                    lab-light)
      +list-dark-theme  '(doom-one
                                    doom-vibrant
                                    doom-acario-dark
                                    doom-city-lights
                                    doom-challenger-deep
                                    doom-dark+
                                    doom-dracula
                                    doom-gruvbox
                                    doom-horizon
                                    doom-Iosvkem
                                    doom-laserwave
                                    doom-material
                                    doom-molokai
                                    doom-monokai-classic
                                    doom-monokai-pro
                                    doom-monokai-spectrum
                                    doom-moonlight
                                    doom-oceanic-next
                                    doom-palenight
                                    doom-peacock
                                    doom-rouge
                                    doom-snazzy
                                    doom-spacegrey
                                    doom-tomorrow-night
                                    doom-vibrant
                                    srcery
                                    flucui-dark
                                    lab-dark))

(add-hook! doom-load-theme :append
  (when (and (featurep 'solaire-mode)
             (not (string-prefix-p "doom-" (symbol-name doom-theme))))
    (set-face-background 'solaire-hl-line-face nil)
    (set-face-background 'solaire-default-face nil)))

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
  :when IS-MAC
  :custom
  (auto-dark-emacs/light-theme +list-light-theme)
  (auto-dark-emacs/dark-theme +list-dark-theme)
  :config
  (add-hook! after-init
             :append
             #'auto-dark-emacs/check-and-set-dark-mode))


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
  (telega-chat-mode . emojify-mode))

;; enable emacs27+ build-in fill-column
(when (>= emacs-major-version 27)
  (add-hook! (text-mode prog-mode conf-mode)
             #'display-fill-column-indicator-mode))
