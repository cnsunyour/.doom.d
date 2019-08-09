;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

;; 增加自定义的load-path路径
;; (add-to-list 'load-path (expand-file-name "~/git/aweshell"))
;; (add-to-list 'load-path (expand-file-name "~/git/awesome-tab"))
;; (add-to-list 'load-path (expand-file-name "~/git/awesome-pair"))
(add-to-list 'load-path (expand-file-name "~/git/company-english-helper"))
(add-to-list 'load-path (expand-file-name "~/git/insert-translated-name"))
;; (add-to-list 'load-path (expand-file-name "~/git/highlight-matching-tag"))
;; (add-to-list 'load-path (expand-file-name "~/git/instant-rename-tag"))
(add-to-list 'load-path (expand-file-name "~/git/sdcv"))
(add-to-list 'load-path (expand-file-name "~/git/snails"))
(add-to-list 'load-path (expand-file-name "~/git/fuz.el"))
(add-to-list 'load-path (expand-file-name "~/git/theme-changer"))

;; 让flycheck检查载入el文件时从load-path里搜索
(setq flycheck-emacs-lisp-load-path 'inherit)

(load! "+bindings")
(load! "+chinese")
(load! "+calendar")
(load! "+gtd")
(load! "+myblog")
(load! "+pretty_src_block")

(def-package! exec-path-from-shell
  :config
  (setq exec-path-from-shell-arguments '("-l")))

;; load packages related to org-mode
(def-package! org-pomodoro
  :commands org-pomodoro)
(def-package! counsel-org-clock
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
(setq org-latex-to-pdf-process '("xelatex -interaction nonstopmode %f"
                                 "xelatex -interaction nonstopmode %f"))


;; 猫神出的很好用的多标签管理插件
;; (require 'awesome-tab)
;; (awesome-tab-mode t)
;; (map! (:g "s-[" #'awesome-tab-backward-tab)
;;       (:g "s-]" #'awesome-tab-forward-tab)
;;       (:g "s-{" #'awesome-tab-select-beg-tab)
;;       (:g "s-}" #'awesome-tab-select-end-tab)
;;       (:g "s-1" #'awesome-tab-select-visible-tab)
;;       (:g "s-2" #'awesome-tab-select-visible-tab)
;;       (:g "s-3" #'awesome-tab-select-visible-tab)
;;       (:g "s-4" #'awesome-tab-select-visible-tab)
;;       (:g "s-5" #'awesome-tab-select-visible-tab)
;;       (:g "s-6" #'awesome-tab-select-visible-tab)
;;       (:g "s-7" #'awesome-tab-select-visible-tab)
;;       (:g "s-8" #'awesome-tab-select-visible-tab)
;;       (:g "s-9" #'awesome-tab-select-visible-tab)
;;       (:g "s-0" #'awesome-tab-select-visible-tab))

;; (require 'aweshell)
;; (defun cnsunyour/call-aweshell-new ()
;;   (interactive)
;;   (progn
;;     (aweshell-new)
;;     (delete-other-windows)))
;; (map! :g "s-'" #'cnsunyour/call-aweshell-new)

;; 英文自动补全和翻译，激活命令toggle-company-english-helper
(require 'company-english-helper)

;; 输入insert-translated-name-insert激活命令，可以输入中文后按空格翻译成英文插入当前位置。
(require 'insert-translated-name)

;; 翻译当前单词
(require 'sdcv)
(map! (:g "\C-cd" #'sdcv-search-pointer+)
      (:g "\C-cD" #'sdcv-search-pointer))
;; (setq sdcv-say-word-p t)        ;; 是否读出语音
(setq sdcv-dictionary-data-dir (expand-file-name "~/.stardict/dic"))
(setq sdcv-dictionary-simple-list     ;setup dictionary list for simple search
      '("懒虫简明英汉词典"
        "懒虫简明汉英词典"
        "朗道英汉字典5.0"
        "朗道汉英字典5.0"))
(setq sdcv-dictionary-complete-list   ;setup dictionary list for complete search
      '("懒虫简明英汉词典"
        "懒虫简明汉英词典"
        "朗道英汉字典5.0"
        "朗道汉英字典5.0"
        "21世纪英汉汉英双向词典"
        "牛津英汉双解美化版"
        "英汉汉英专业词典"
        "新世纪英汉科技大词典"
        "现代汉语词典"
        "高级汉语大词典"))

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

;; web-mode下标签改名和高亮插件
;; (require 'instant-rename-tag)
;; (require 'highlight-matching-tag)
;; (highlight-matching-tag 1)

;; A modern, easy-to-expand fuzzy search framework
;; M-x snails or M-x snails-search-point
(require 'snails)
(map! (:leader (:desc "Snails" :gnv "os" #'snails))
      (:map snails-mode-map
        :e "s-h" #'snails-quit
        :e "s-n" #'snails-select-next-item
        :e "s-p" #'snails-select-prev-item
        :e "s-j" #'snails-select-next-backend
        :e "s-k" #'snails-select-prev-backend))
(add-hook! 'snails-mode-hook #'(lambda () (evil-emacs-state)))

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
(def-package! company-tabnine
  :when (featurep! :completion company)
  :config
  (add-to-list 'company-backends #'company-tabnine)
  (set-company-backend! '(c-mode
                          c++-mode
                          java-mode
                          swift-mode
                          haskell-mode
                          emacs-lisp-mode
                          lisp-mode
                          sh-mode
                          perl-mode
                          php-mode
                          python-mode
                          go-mode
                          lua-mode
                          ruby-mode
                          rust-mode
                          js-mode
                          css-mode
                          web-mode)
    'company-tabnine)

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
;; (setq-default fill-column 100)
;; (setq-local fill-column 100)

;; 使用相对行号
(setq display-line-numbers-type 'relative)

;; 调整启动时的窗口大小
(pushnew! initial-frame-alist '(width . 200) '(height . 55))

;; 在Mac平台, Emacs不能进入Mac原生的全屏模式,否则会导致 `make-frame' 创建时也集
;; 成原生全屏属性后造成白屏和左右滑动现象. 所以先设置 `ns-use-native-fullscreen'
;; 和 `ns-use-fullscreen-animation' 禁止Emacs使用Mac原生的全屏模式. 而是采用传统
;; 的全屏模式, 传统的全屏模式, 只会在当前工作区全屏,而不是切换到Mac那种单独的全
;; 屏工作区,这样执行 `make-frame' 先关代码或插件时,就不会因为Mac单独工作区左右滑
;; 动产生的bug.
;;
;; Mac平台下,不能直接使用 `set-frame-parameter' 和 `fullboth' 来设置全屏,那样也
;; 会导致Mac窗口管理器直接把Emacs窗口扔到单独的工作区, 从而对 `make-frame' 产生
;; 同样的Bug. 所以, 启动的时候通过`set-frame-parameter' 和 `maximized' 先设置
;; Emacs为最大化窗口状态, 启动5秒以后再设置成全屏状态, Mac就不会移动Emacs窗口到
;; 单独的工作区, 最终解决Mac平台下原生全屏窗口导致 `make-frame' 左右滑动闪烁的问
;; 题.
(setq ns-use-native-fullscreen nil)
(setq ns-use-fullscreen-animation nil)
(run-at-time "1sec" nil
             (lambda ()
               (let ((fullscreen (frame-parameter (selected-frame) 'fullscreen)))
                 ;; If emacs has in fullscreen status, maximized window first,
                 ;; drag from Mac's single space.
                 (when (memq fullscreen '(fullscreen fullboth))
                   (set-frame-parameter (selected-frame) 'fullscreen 'maximized))
                 ;; Manipulating a frame without waiting for the fullscreen
                 ;; animation to complete can cause a crash, or other unexpected
                 ;; behavior, on macOS (bug#28496).
                 (when (featurep 'cocoa) (sleep-for 0.5))
                 ;; Call `toggle-frame-fullscreen' to fullscreen emacs.
                 (toggle-frame-fullscreen))))


;; 每天根据日出日落时间换主题
(require 'theme-changer)
(change-theme 'doom-nord-light 'doom-Iosvkem)


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
