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


(when (display-graphic-p)
  (make-face 'my-telega-face)

  (defun telega-buffer-face-mode-variable ()
    (interactive)
    (if (local-variable-p 'buffer-face-mode-face)
        (setq buffer-face-mode-face 'my-telega-face)
      (setq-local buffer-face-mode-face 'my-telega-face))
    (unless (local-variable-p 'face-font-rescale-alist)
      (make-local-variable 'face-font-rescale-alist))
    (dolist (emoji doom-emoji-fallback-font-families)
      (add-to-list 'face-font-rescale-alist (cons (concat "-" emoji "-") 0.75)))
    (buffer-face-mode))

  (add-hook 'telega-chat-mode-hook 'telega-buffer-face-mode-variable))
