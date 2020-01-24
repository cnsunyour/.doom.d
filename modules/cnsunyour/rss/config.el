;;; cnsunyour/rss/config.el -*- lexical-binding: t; -*-


;; custom elfeed's configuration
(after! elfeed
  (set-evil-initial-state! '(elfeed-search-mode elfeed-show-mode) 'emacs)
  (map! (:leader :desc "Elfeed" "ve" #'elfeed)
        (:map elfeed-search-mode-map
          "q" #'elfeed-kill-buffer
          (:when (featurep! :editor evil +everywhere)
            :n "R" #'elfeed-search-fetch))))
