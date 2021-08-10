;;; ~/.doom.d/+translate.el -*- lexical-binding: t; -*-

;; 英文自动补全和翻译，激活命令toggle-company-english-helper
(use-package! company-english-helper
  :defer t
  :commands (toggle-company-english-helper)
  :init
  (map! :leader
        :prefix ("y" . "Translate")
        "M" #'toggle-company-english-helper))

;; 输入insert-translated-name-insert激活命令，可以输入中文后按空格翻译成英文插入当前位置。
(use-package! insert-translated-name
  :defer t
  :commands (insert-translated-name-insert-original-translation)
  :init
  (map! :leader
        :prefix ("y" . "Translate")
        "m" #'insert-translated-name-insert-original-translation))

;; sdcv翻译当前单词
(use-package! sdcv
  :defer t
  :commands (sdcv-search-pointer+
             sdcv-search-pointer)
  :init
  (map! :leader
        :prefix ("y" . "Translate")
        "d" #'sdcv-search-pointer+
        "D" #'sdcv-search-pointer)
  :config
  (setq sdcv-tooltip-timeout 30)
  (set-popup-rule! (regexp-quote sdcv-buffer-name) :side 'right :size 0.4 :select t)
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
          ;; "KDic11万英汉词典"
          ;; "XDICT英汉辞典"
          ;; "XDICT汉英辞典"
          "21世纪英汉汉英双向词典"
          ;; "21世纪双语科技词典"
          "牛津英汉双解美化版"
          ;; "英汉汉英专业词典"
          ;; "新世纪英汉科技大词典"
          ;; "新世纪汉英科技大词典"
          "现代汉语词典"
          "高级汉语大词典")))

;; 有道词典
(use-package! youdao-dictionary
  :defer t
  :init
  (map! :leader
        :prefix ("y" . "Translate")
        "y" #'youdao-dictionary-search-at-point-posframe
        "Y" #'youdao-dictionary-search-at-point
        "p" #'youdao-dictionary-play-voice-at-point
        "P" #'youdao-dictionary-play-voice-from-input)
  :config
  (set-popup-rule! (regexp-quote youdao-dictionary-buffer-name) :side 'right :size 0.4 :select t)
  ;; 设定youdao api id和key
  ;; (let ((credentials (auth-source-user-and-password "youdao-api")))
  ;;   (setq youdao-dictionary-app-key (car credentials)
  ;;         youdao-dictionary-secret-key (cadr credentials)))
  ;; Enable Cache
  (setq url-automatic-caching t)
  ;; Enable Chinese word segmentation support (支持中文分词)
  (setq youdao-dictionary-use-chinese-word-segmentation t))

(use-package! fanyi
  :init
  (map! :leader
        :prefix ("y" . "Translate")
        "f" #'fanyi-dwim)
  :config
  (set-popup-rule! (regexp-quote fanyi-buffer-name) :side 'right :size 0.4 :select t)
  )
