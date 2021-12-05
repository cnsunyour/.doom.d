;;; cnsunyour/telega/+telega-ext.el -*- lexical-binding: t; -*-

;; Highlight current line in root buffer
(defun lg-telega-root-mode ()
  (hl-line-mode))
(defun lg-telega-chat-update (chat)
  (with-telega-root-buffer
    (hl-line-highlight)))
(add-hook 'telega-chat-update-hook 'lg-telega-chat-update)
(add-hook 'telega-root-mode-hook 'lg-telega-root-mode)

;; Outline unmuted chats using braces
(setq telega-chat-button-brackets
      (list (list '(and (not unmuted) (type private))
                  (propertize "{" 'face 'shadow)
                  (propertize "}" 'face 'shadow))
            (list '(and (not unmuted) (type basicgroup))
                  (propertize "(" 'face 'shadow)
                  (propertize ")" 'face 'shadow))
            (list '(and (not unmuted) (type supergroup))
                  (propertize "[" 'face 'shadow)
                  (propertize "]" 'face 'shadow))
            (list '(and (not unmuted) (type channel))
                  (propertize "<" 'face 'shadow)
                  (propertize ">" 'face 'shadow))
            (list '(type private)    "{" "}")
            (list '(type basicgroup) "(" ")")
            (list '(type supergroup) "[" "]")
            (list '(type channel)    "<" ">")
            (list 'all               "[" "]")))


(make-face 'my-align-by-sarasa)
(set-face-font 'my-align-by-sarasa (font-spec :family "Sarasa Mono SC"))
(defun my-align-with-sarasa-font ()
  (interactive)
  (when (member "Sarasa Mono SC" (font-family-list))
    (set (make-variable-buffer-local 'buffer-face-mode-face) 'my-align-by-sarasa)
    (make-variable-buffer-local 'face-font-rescale-alist)
    ;; make symbols smaller
    (add-to-list 'face-font-rescale-alist '("-Noto Color Emoji-" . 0.8))
    (add-to-list 'face-font-rescale-alist '("-Apple Color Emoji-" . 0.8))
    (add-to-list 'face-font-rescale-alist '("-Noto Sans Symbols-" . 0.8))
    (add-to-list 'face-font-rescale-alist '("-Noto Sans Symbols2-" . 0.8))
    (add-to-list 'face-font-rescale-alist '("-Symbola-" . 0.8))
    (buffer-face-mode 1)))
(add-hook! '(telega-root-mode-hook telega-chat-mode-hook)
           #'my-align-with-sarasa-font)
