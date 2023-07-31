;;; ~/.doom.d/+translate.el -*- lexical-binding: t; -*-


(use-package! fanyi
  :defer t
  :init
  (map! :leader
        :prefix ("y" . "Translate")
        "f" #'fanyi-dwim2
        "F" #'fanyi-dwim)
  :config
  (set-popup-rule! (regexp-quote fanyi-buffer-name) :side 'right :size 80 :select t :quit t))


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
  (set-popup-rule! (regexp-quote gts-buffer-name) :side 'right :size 80 :select t :quit t)
  (setq gts-translate-list '(("en" "zh") ("zh" "en"))
        gts-default-translator
        (gts-translator
         :picker (gts-noprompt-picker)
         :engines (list (gts-bing-engine)
                        (gts-google-engine)
                        (gts-google-rpc-engine))
         :render (gts-buffer-render)))
  (defvar my-gts-translator
    (gts-translator
     :picker (gts-prompt-picker)
     :engines (list (gts-bing-engine)
                    (gts-google-engine)
                    (gts-google-rpc-engine))
     :render (gts-buffer-render)))
  (defun my-gts-do-translate()
    (interactive)
    (gts-translate my-gts-translator)))
