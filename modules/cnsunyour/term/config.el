;;; cnsunyour/term/config.el -*- lexical-binding: t; -*-

(after! vterm
  (when (featurep! :editor evil)
    (evil-set-initial-state 'vterm-mode 'emacs)))

(after! eshell
  (when (featurep! :editor evil)
    (evil-set-initial-state 'eshell-mode 'emacs)))
