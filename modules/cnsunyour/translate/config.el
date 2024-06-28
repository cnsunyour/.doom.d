;;; ~/.doom.d/+translate.el -*- lexical-binding: t; -*-

;; 有道词典
(use-package! youdao-dictionary
  :defer t
  :init
  (map! :leader
        :prefix ("y" . "Translate")
        "y" #'youdao-dictionary-search-at-point-posframe
        "Y" #'youdao-dictionary-search-at-point)
  :config
  (set-popup-rule! (regexp-quote youdao-dictionary-buffer-name)
    :side 'right :size 100 :select t :quit 'current)
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
  (set-popup-rule! (regexp-quote fanyi-buffer-name)
    :side 'right :size 100 :select t :quit 'current))


(use-package! go-translate
  :defer t
  :init
  (map! :leader
        :prefix ("y" . "Translate")
        "g" #'gt-do-translate
        "G" (cmd! (gt-do-translate t)))
  :config
  (set-popup-rule! (regexp-quote gt-buffer-render-buffer-name)
    :side 'right :size 100 :select t :quit 'current)
  (load! "gt-engine-azure-openai.el")
  (setq gt-preset-translators
        `((ts-word . ,(gt-translator
                       :taker (gt-taker :langs '(en zh) :text 'word :pick nil)
                       :engines (list (gt-deepl-engine :if 'not-word)
                                      (gt-azure-openai-engine :if 'not-word :stream t)
                                      (gt-bing-engine :if 'not-word)
                                      (gt-google-engine :if 'not-word)
                                      (gt-youdao-dict-engine :if '(and word (or src:zh tgt:zh)))
                                      (gt-youdao-suggest-engine :if '(and word src:en)))
                       :render (gt-buffer-render)))
          (ts-sentence . ,(gt-translator
                           :taker (gt-taker :langs '(en zh) :text 'sentence :pick nil)
                           :engines (list (gt-deepl-engine :if 'not-word)
                                          (gt-azure-openai-engine :if 'not-word :stream t)
                                          (gt-bing-engine :if 'not-word)
                                          (gt-google-engine :if 'not-word)
                                          (gt-youdao-dict-engine :if '(and word (or src:zh tgt:zh)))
                                          (gt-youdao-suggest-engine :if '(and word src:en)))
                           :render (gt-buffer-render)))
          (ts-paragraph . ,(gt-translator
                            :taker (gt-taker :langs '(en zh) :text 'paragraph :pick nil)
                            :engines (list (gt-deepl-engine :if 'not-word)
                                           (gt-azure-openai-engine :if 'not-word :stream t)
                                           (gt-bing-engine :if 'not-word)
                                           (gt-google-engine :if 'not-word)
                                           (gt-youdao-dict-engine :if '(and word (or src:zh tgt:zh)))
                                           (gt-youdao-suggest-engine :if '(and word src:en)))
                            :render (gt-buffer-render)))
          (ts-buffer . ,(gt-translator
                         :taker (gt-taker :langs '(en zh) :text 'buffer :pick 'paragraph)
                         :engines (list (gt-deepl-engine :if 'not-word)
                                        (gt-azure-openai-engine :if 'not-word :stream t)
                                        (gt-bing-engine :if 'not-word)
                                        (gt-google-engine :if 'not-word)
                                        (gt-youdao-dict-engine :if '(and word (or src:zh tgt:zh)))
                                        (gt-youdao-suggest-engine :if '(and word src:en)))
                         :render (gt-buffer-render))))))
