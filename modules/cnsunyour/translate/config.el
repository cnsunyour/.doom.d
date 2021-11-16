;;; ~/.doom.d/+translate.el -*- lexical-binding: t; -*-

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
  (set-popup-rule! (regexp-quote youdao-dictionary-buffer-name) :side 'right :size 0.5 :select t)
  ;; 设定youdao api id和key
  ;; (let ((credentials (auth-source-user-and-password "youdao-api")))
  ;;   (setq youdao-dictionary-app-key (car credentials)
  ;;         youdao-dictionary-secret-key (cadr credentials)))
  ;; Enable Cache
  (setq url-automatic-caching t)
  ;; Enable Chinese word segmentation support (支持中文分词)
  (setq youdao-dictionary-use-chinese-word-segmentation t))


(use-package! fanyi
  :defer t
  :init
  (map! :leader
        :prefix ("y" . "Translate")
        "f" #'fanyi-dwim2
        "F" #'fanyi-dwim)
  :config
  (set-popup-rule! (regexp-quote fanyi-buffer-name) :side 'right :size 0.5 :select t))


(use-package! go-translate
  :defer t
  :commands
  (gts-do-translate
   my-gts-do-translate)
  :init
  (map! :leader
        :prefix ("y" . "Translate")
        "g" #'gts-do-translate
        "G" #'my-gts-do-translate)
  :config
  (set-popup-rule! (regexp-quote gts-buffer-name) :side 'right :size 0.5 :select t)
  (setq gts-translate-list '(("en" "zh") ("zh" "en"))
        gts-default-translator
        (gts-translator
         :picker (gts-noprompt-picker)
         :engines (list (gts-google-engine))
         :render (gts-buffer-render)))
  (defvar my-gts-translator
    (gts-translator
     :picker (gts-prompt-picker)
     :engines (list (gts-google-engine))
     :render (gts-buffer-render)))
  (defun my-gts-do-translate()
    (interactive)
    (gts-translate my-gts-translator)))
