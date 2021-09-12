;;; ~/.doom.d/+translate.el -*- lexical-binding: t; -*-

;; 有道词典
(use-package! youdao-dictionary
  :defer t
  :init
  (map! :leader
        :prefix ("y" . "Translate")
        "y" #'youdao-dictionary-search-at-point+
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
