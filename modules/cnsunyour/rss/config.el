;;; cnsunyour/rss/config.el -*- lexical-binding: t; -*-


;; custom elfeed's configuration
(when (fboundp 'elfeed)
  (map! :leader :desc "Elfeed" "ve" #'elfeed))
(after! elfeed
  (set-evil-initial-state! '(elfeed-search-mode elfeed-show-mode) 'emacs)
  (map! :map elfeed-search-mode-map
        "q" #'elfeed-kill-buffer
        (:when (featurep! :editor evil +everywhere)
          :n "R" #'elfeed-search-fetch)))
