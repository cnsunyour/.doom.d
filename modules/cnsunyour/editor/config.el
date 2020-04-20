;;; cnsunyour/editor/config.el -*- lexical-binding: t; -*-

;; Edit comment or docstring or code block inside them with your favorite mode.
(use-package! separedit
  :defer t
  :custom
  (separedit-default-mode 'markdown-mode)
  :init
  (map! :map prog-mode-map "C-c '" #'separedit)
  :config
  (set-popup-rule! "^\\*edit-indirect " :side 'right :size .5 :select t :quit nil))
