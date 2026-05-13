;;; ~/.doom.d/+translate.el -*- lexical-binding: t; -*-

(use-package! gt
  :ensure t
  :bind
  ("C-c y" . gt-translate)
  :init
  (map! :leader
        "y" #'gt-translate)
  :custom
  (gt-chatgpt-host "https://api.deepseek.com")
  (gt-chatgpt-model "deepseek-v4-flash")
  :config
  (if (modulep! :editor evil)
      (add-hook! '(gt-buffer-render-output-hook
                   gt-posframe-pin-render-output-hook
                   gt-posframe-pop-render-output-hook)
                 #'evil-emacs-state))
  (set-popup-rule! (regexp-quote gt-buffer-render-buffer-name)
    :side 'right :size 100 :select t :quit 'current)
  (setq gt-langs '(en zh)
        gt-preset-translators
        `((ts-0 . ,(gt-translator
                    :taker   (list (gt-taker :pick nil :if 'selection)
                                   (gt-taker :text 'paragraph :if (lambda (&rest _) (derived-mode-p 'Info-mode)))
                                   (gt-taker :text 'buffer :pick 'paragraph :if 'read-only)
                                   (gt-taker))
                    :engines (list (gt-deepl-engine :if 'not-word)
                                   (gt-chatgpt-engine :if 'not-word)
                                   (gt-youdao-dict-engine :if '(and word (or src:zh tgt:zh)))
                                   (gt-youdao-suggest-engine :if '(and word src:en)))
                    :render  (list (gt-posframe-pop-render :if '(or word selection))
                                   (gt-overlay-render :if 'read-only)
                                   (gt-insert-render :if (lambda (&rest _) (member (buffer-name) '("COMMIT_EDITMSG"))))
                                   (gt-buffer-render))))
          (ts-1 . ,(gt-translator
                    :taker   (list (gt-taker :text 'paragraph))
                    :engines (list (gt-deepl-engine)
                                   (gt-chatgpt-engine))
                    :render  (list (gt-buffer-render))))
          (ts-2 . ,(gt-translator
                    :taker   (list (gt-taker :text 'buffer))
                    :engines (list (gt-deepl-engine)
                                   (gt-chatgpt-engine))
                    :render  (list (gt-buffer-render)))))))
