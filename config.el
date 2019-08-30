;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
(setq user-full-name "Sunn Yao"
      user-mail-address "sunyour@gmail.com"
      epa-file-encrypt-to user-mail-address)

;; 设置我所在地方的经纬度，calendar里有个功能是日月食的预测，和经纬度相联系的。
;; 让emacs能计算日出日落的时间，在 calendar 上用 S 即可看到
;; 另外根据日出日落时间切换主题也需要经纬度
(setq calendar-location-name "北京")
(setq calendar-latitude +39.9055472)
(setq calendar-longitude +116.3887056)

;; 让flycheck检查载入el文件时从load-path里搜索
(setq flycheck-emacs-lisp-load-path 'inherit)

(use-package! exec-path-from-shell
  :config
  (setq exec-path-from-shell-arguments '("-l")))

(load! "+bindings")
(load! "+chinese")
(load! "+calendar")
(load! "+gtd")
(load! "+myblog")
(load! "+pretty_src_block")
(load! "+translate")
(load! "+manateelazycat")

(use-package! fuz
  :config
  (unless (require 'fuz-core nil t)
    (fuz-build-and-load-dymod)))

(use-package! pinentry
  :config
  (pinentry-start))

;; load packages related to org-mode
(use-package! org-pomodoro
  :commands org-pomodoro)
(use-package! counsel-org-clock
  :commands (counsel-org-clock-context counsel-org-clock-history))

;; 设置latex编辑tex文件时用skim同步显示pdf
(setq TeX-source-correlate-mode t)
(setq TeX-source-correlate-start-server t)
(setq TeX-source-correlate-method 'synctex)
(setq TeX-view-program-list
      '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
(setq TeX-view-program-selection '((output-pdf "Skim")))

;; 使用xelatex一步生成PDF
(setq org-latex-pdf-process '("xelatex -interaction nonstopmode %f"
                              "xelatex -interaction nonstopmode %f"))

;; 在Eshell中发送桌面通知
(require 'alert)
(defun eshell-command-alert (process status)
  "Send `alert' with severity based on STATUS when PROCESS finished."
  (let* ((cmd (process-command process))
         (buffer (process-buffer process))
         (msg (format "%s: %s" (mapconcat 'identity cmd " ") status)))
    (if (string-prefix-p "finished" status)
        (alert msg :buffer buffer :severity 'normal)
      (alert msg :buffer buffer :severity 'urgent))))
(add-hook 'eshell-kill-hook #'eshell-command-alert)
(alert-add-rule :status '(buried) ;only send alert when buffer not visible
                :mode 'eshell-mode
                :style 'notifications)

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
(map! (:g "M-i" 'symbol-overlay-put)
      (:g "M-n" 'symbol-overlay-switch-forward)
      (:g "M-p" 'symbol-overlay-switch-backward)
      (:g "<f7>" 'symbol-overlay-mode)
      (:g "<f8>" 'symbol-overlay-remove-all))

;; tabnine，一个非常牛的补全插件
(use-package! company-tabnine
  :when (featurep! :completion company)
  :config
  (add-to-list 'company-backends #'company-tabnine)
  (set-company-backend! 'prog-mode
    'company-tabnine 'company-capf 'company-yasnippet)
  ;; (setq +lsp-company-backend '(company-lsp :with company-tabnine :separate))
  (setq +lsp-company-backend '(company-tabnine :with company-lsp :separate))

  ;; Trigger completion immediately.
  ;; (setq company-idle-delay 0)

  ;; Number the candidates (use M-1, M-2 etc to select completions).
  ;; (setq company-show-numbers t)

  ;; Use the tab-and-go frontend.
  ;; Allows TAB to select and complete at the same time.
  (company-tng-configure-default)
  (setq company-frontends
        '(company-tng-frontend
          company-pseudo-tooltip-frontend
          company-echo-metadata-frontend))
  )

;; define environmental variable for some works
(setenv "PKG_CONFIG_PATH"
        (concat
         "/usr/local/opt/libffi/lib/pkgconfig" path-separator
         "/usr/local/opt/qt/lib/pkgconfig" path-separator
         "/usr/local/opt/nss/lib/pkgconfig" path-separator
         (getenv "PKG_CONFIG_PATH")))

;; 80列太窄，120列太宽，看着都不舒服，100列正合适
(setq-default fill-column 100)
(setq-local fill-column 100)

;; 使用相对行号
(setq display-line-numbers-type 'relative)

;; 调整Mac下窗口和全屏显示方式
(when IS-MAC
  (setq ns-use-thin-smoothing t)
  (setq ns-use-native-fullscreen nil)
  (setq ns-use-fullscreen-animation nil))

;; 调整启动时窗口大小/最大化/全屏
;; (pushnew! initial-frame-alist '(width . 200) '(height . 55))
(add-hook 'window-setup-hook #'toggle-frame-maximized t)
;; (add-hook 'window-setup-hook #'toggle-frame-fullscreen t)

;; 每天根据日出日落时间换主题
(use-package! theme-changer
  :config
  (change-theme '(doom-nord-light
                  doom-opera-light
                  doom-solarized-light)
                '(doom-one
                  doom-city-lights
                  doom-Iosvkem
                  doom-nord
                  doom-peacock)))

;; elisp eval
(defun eval-this-buffer ()
  (interactive)
  (eval-buffer nil (get-buffer-create "output"))
  (switch-to-buffer-other-window "output"))

;; 显示儿子的成长时间
(defun twinkle-live-time ()
  "Display the live time of my son."
  (interactive)
  (let* ((birth-time (encode-time 0 43 13 16 9 2013))
         (live-time  (time-subtract (current-time) birth-time))
         (lt-secs    (float-time live-time)))
    (message
     (format "Twinkle: %d days; %.2f months; %.2f weeks; -- %s"
             (floor (/ lt-secs 86400))
             (/ lt-secs 2628000) ;; 1 y = 12 m, 1 m ~= 30.4166667 d
             (/ lt-secs 604800)
             (format-seconds "%Y, %D, %H, %M%z" lt-secs)))))
