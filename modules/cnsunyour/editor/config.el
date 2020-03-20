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

;; Symbol Overlay 多关键字高亮插件
;; Highlight symbols with overlays while providing a keymap for various
;; operations about highlighted symbols. It was originally inspired by
;; the package highlight-symbol. The fundamental difference is that in
;; symbol-overlay every symbol is highlighted by the Emacs built-in
;; function overlay-put rather than the font-lock mechanism used in
;; highlight-symbol.
;; Default key-bindings defined in symbol-overlay-map:
;; "i" -> symbol-overlay-put
;; "n" -> symbol-overlay-jump-next
;; "p" -> symbol-overlay-jump-prev
;; "w" -> symbol-overlay-save-symbol
;; "t" -> symbol-overlay-toggle-in-scope
;; "e" -> symbol-overlay-echo-mark
;; "d" -> symbol-overlay-jump-to-definition
;; "s" -> symbol-overlay-isearch-literally
;; "q" -> symbol-overlay-query-replace
;; "r" -> symbol-overlay-rename
(map! "M-i" 'symbol-overlay-put
      "M-n" 'symbol-overlay-switch-forward
      "M-p" 'symbol-overlay-switch-backward
      "<f7>" 'symbol-overlay-mode
      "<f8>" 'symbol-overlay-remove-all)
