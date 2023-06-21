;;; cnsunyour/term/config.el -*- lexical-binding: t; -*-

(after! vterm
  (when (modulep! :editor evil)
    (cl-pushnew 'vterm +evil-collection-disabled-list :test #'equal)
    (setq evil-collection-mode-list (remove 'vterm evil-collection-mode-list))
    (evil-set-initial-state 'vterm-mode 'emacs)))

(after! eshell
  (when (modulep! :editor evil)
    (evil-set-initial-state 'eshell-mode 'emacs)))
