;;; cnsunyour/editor/config.el -*- lexical-binding: t; -*-

;; Edit comment or docstring or code block inside them with your favorite mode.
(use-package! comment-edit
  :defer t
  :custom
  (comment-edit-default-mode 'markdown-mode)
  :init
  (map! :map prog-mode-map "C-c '" #'comment-edit)
  :config
  (set-popup-rule! "^\\*edit-indirect " :side 'right :size .4 :select t :quit nil))
