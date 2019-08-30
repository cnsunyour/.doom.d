;;; ~/.doom.d/+manateelazycat.el -*- lexical-binding: t; -*-

;; 英文自动补全和翻译，激活命令toggle-company-english-helper
(use-package! company-english-helper
  :defer t
  :commands toggle-company-english-helper
  :init
  (map! :leader :g "yM" #'toggle-company-english-helper))

;; 输入insert-translated-name-insert激活命令，可以输入中文后按空格翻译成英文插入当前位置。
(use-package! insert-translated-name
  :defer t
  :commands insert-translated-name-insert-original-translation
  :init
  (map! :leader :g "ym" #'insert-translated-name-insert-original-translation))

;; sdcv翻译当前单词
(use-package! sdcv
  :defer t
  :commands sdcv-search-pointer+ sdcv-search-pointer
  :init
  (map! :leader
        :g "yd" #'sdcv-search-pointer+
        :g "yD" #'sdcv-search-pointer)
  :config
  (set-face-background 'sdcv-tooltip-face nil)
  (set-face-foreground 'sdcv-tooltip-face nil)
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
          "KDic11万英汉词典"
          "XDICT英汉辞典"
          "XDICT汉英辞典"
          "21世纪英汉汉英双向词典"
          "牛津英汉双解美化版"
          "英汉汉英专业词典"
          "新世纪英汉科技大词典"
          "新世纪汉英科技大词典"
          "现代汉语词典"
          "高级汉语大词典")))

(use-package! awesome-tab
  :config
  (awesome-tab-mode t)
  (map! :g "s-[" #'awesome-tab-backward-tab
        :g "s-]" #'awesome-tab-forward-tab
        :g "s-{" #'awesome-tab-select-beg-tab
        :g "s-}" #'awesome-tab-select-end-tab
        :g "s-1" #'awesome-tab-select-visible-tab
        :g "s-2" #'awesome-tab-select-visible-tab
        :g "s-3" #'awesome-tab-select-visible-tab
        :g "s-4" #'awesome-tab-select-visible-tab
        :g "s-5" #'awesome-tab-select-visible-tab
        :g "s-6" #'awesome-tab-select-visible-tab
        :g "s-7" #'awesome-tab-select-visible-tab
        :g "s-8" #'awesome-tab-select-visible-tab
        :g "s-9" #'awesome-tab-select-visible-tab
        :g "s-0" #'awesome-tab-select-visible-tab))

(use-package! aweshell
  :defer t
  :init
  (map! :g "s-'" #'cnsunyour/call-aweshell-new)
  :config
  (defun cnsunyour/call-aweshell-new ()
    (interactive)
    (progn
      (aweshell-new)
      (delete-other-windows))))

;; web-mode下标签改名和高亮插件
(use-package! instant-rename-tag :defer t)
(use-package! highlight-matching-tag
  :defer t
  :config
  (highlight-matching-tag 1))

;; A modern, easy-to-expand fuzzy search framework
;; M-x snails or M-x snails-search-point
(use-package! snails
  :load-path "/Users/yaohui/git/snails"
  :defer t
  :commands snails snails-search-point
  :init
  (map! :leader
        (:g "os" #'snails)
        (:g "oS" #'snails-search-point))
  :config
  (add-to-list 'snails-default-backends #'snails-backend-current-buffer t)
  (add-hook 'snails-mode-hook (lambda () (evil-emacs-state))))
