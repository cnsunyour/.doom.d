;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

;; 增加自定义的.el文件路径
(add-to-list 'load-path (expand-file-name "~/.doom.d/elisp"))

;; 让flycheck检查载入el文件时从load-path里搜索
(setq flycheck-emacs-lisp-load-path 'inherit)

(load! "+bindings")
(load! "+chinese")
(load! "+calendar")
(load! "+gtd")
(load! "+myblog")
(load! "+pretty_src_block")

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
;; (global-set-key (kbd "s-[") 'awesome-tab-backward-tab)
;; (global-set-key (kbd "s-]") 'awesome-tab-forward-tab)
;; (global-set-key (kbd "s-{") 'awesome-tab-select-beg-tab)
;; (global-set-key (kbd "s-}") 'awesome-tab-select-end-tab)
;; (global-set-key (kbd "s-1") 'awesome-tab-select-visible-tab)
;; (global-set-key (kbd "s-2") 'awesome-tab-select-visible-tab)
;; (global-set-key (kbd "s-3") 'awesome-tab-select-visible-tab)
;; (global-set-key (kbd "s-4") 'awesome-tab-select-visible-tab)
;; (global-set-key (kbd "s-5") 'awesome-tab-select-visible-tab)
;; (global-set-key (kbd "s-6") 'awesome-tab-select-visible-tab)
;; (global-set-key (kbd "s-7") 'awesome-tab-select-visible-tab)
;; (global-set-key (kbd "s-8") 'awesome-tab-select-visible-tab)
;; (global-set-key (kbd "s-9") 'awesome-tab-select-visible-tab)
;; (global-set-key (kbd "s-0") 'awesome-tab-select-visible-tab)

;; 英文自动补全和翻译，激活命令toggle-company-english-helper
(require 'company-english-helper)

;; 输入insert-translated-name-insert激活命令，可以输入中文后按空格翻译成英文插入当前位置。
(require 'insert-translated-name)

;; 翻译当前单词
(require 'sdcv)
(global-set-key "\C-cd" 'sdcv-search-pointer+)
(global-set-key "\C-cD" 'sdcv-search-pointer)
(setq sdcv-say-word-p t)        ;; 是否读出语音
(setq sdcv-dictionary-data-dir (expand-file-name "~/.stardict/dic"))
(setq sdcv-dictionary-simple-list       ;setup dictionary list for simple search
      '("懒虫简明英汉词典"
        "懒虫简明汉英词典"
        "朗道英汉字典5.0"
        "朗道汉英字典5.0"))
(setq sdcv-dictionary-complete-list     ;setup dictionary list for complete search
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
;; (require 'alert)
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
(require 'instant-rename-tag)
(require 'highlight-matching-tag)
(highlight-matching-tag 1)

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
(require 'symbol-overlay)
(global-set-key (kbd "M-i") 'symbol-overlay-put)
(global-set-key (kbd "M-n") 'symbol-overlay-switch-forward)
(global-set-key (kbd "M-p") 'symbol-overlay-switch-backward)
(global-set-key (kbd "<f7>") 'symbol-overlay-mode)
(global-set-key (kbd "<f8>") 'symbol-overlay-remove-all)


;; 从 Emacs 往 OmniFocus 添加任务
(defun omnifocus-capture (name note)
  "Add task to OmniFocus, NAME as the task name and NOTE as the task note."
  (interactive "sTask name: \nsTask note: ")
  (let ((quote-fn
         (lambda (s)
           "Quote S for passing as a string to AppleScript."
           (mapconcat
            (lambda (char)
              (pcase char
                (?\" (string ?\\ ?\"))
                (?\\ (string ?\\ ?\\))
                (_   (string char))))
            s ""))))
    (do-applescript
     (format
      (concat
       "tell front document of application \"Omnifocus\"\n"
       "  make new inbox task with properties {name:\"%s\", note:\"%s\"}\n"
       "end tell")
      (funcall quote-fn name)
      (funcall quote-fn note)))))

(def-package! company-tabnine
  :config
  (add-to-list 'company-backends #'company-tabnine)
  ;; Trigger completion immediately.
  (setq company-idle-delay 0)

  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (setq company-show-numbers t)

  ;; Use the tab-and-go frontend.
  ;; Allows TAB to select and complete at the same time.
  (company-tng-configure-default)
  (setq company-frontends
        '(company-tng-frontend
          company-pseudo-tooltip-frontend
          company-echo-metadata-frontend)))

;; define environmental variable for some works
(setenv "PKG_CONFIG_PATH"
        (concat
         "/usr/local/opt/libffi/lib/pkgconfig" path-separator
         "/usr/local/opt/qt/lib/pkgconfig" path-separator
         "/usr/local/opt/nss/lib/pkgconfig" path-separator
         (getenv "PKG_CONFIG_PATH")))

;; 调整启动时的窗口大小
(pushnew! initial-frame-alist
          '(width . 200)
          '(height . 55))

;; 启动python虚拟环境
;; (pyvenv-workon "myvw-3.7")
