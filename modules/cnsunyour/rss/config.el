;;; cnsunyour/rss/config.el -*- lexical-binding: t; -*-


;; custom elfeed's configuration
(after! elfeed
  (set-evil-initial-state! '(elfeed-search-mode elfeed-show-mode) 'emacs)
  (when (featurep! :editor evil +everywhere)
    (evil-define-key 'normal elfeed-search-mode-map
      "R" #'elfeed-search-fetch))
  (define-key elfeed-search-mode-map "q" #'elfeed-kill-buffer))
