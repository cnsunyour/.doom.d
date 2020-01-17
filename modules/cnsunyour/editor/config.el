;;; cnsunyour/editor/config.el -*- lexical-binding: t; -*-

;; Edit comment or docstring or code block inside them with your favorite mode.
(use-package! comment-edit
  :custom
  (comment-edit-default-mode 'markdown-mode)
  :config
  (define-key prog-mode-map (kbd "C-c '") #'comment-edit))
