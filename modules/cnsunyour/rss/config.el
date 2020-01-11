;;; cnsunyour/rss/config.el -*- lexical-binding: t; -*-


;; custom elfeed's configuration
(after! elfeed
  (add-hook 'elfeed-show-mode-hook #'evil-emacs-state)
  (when (featurep! :editor evil +everywhere)
    (evil-define-key 'normal elfeed-search-mode-map
      "R" #'elfeed-search-fetch)))
