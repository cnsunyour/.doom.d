;;; cnsunyour/term/config.el -*- lexical-binding: t; -*-

(after! vterm
  (when (modulep! :editor evil)
    (evil-set-initial-state 'vterm-mode 'emacs)))

(after! eshell
  (when (modulep! :editor evil)
    (evil-set-initial-state 'eshell-mode 'emacs)))
