;;; cnsunyour/chinese/+rime-probe-english.el -*- lexical-binding: t; -*-

(defun +rime--beancount-p ()
  "Determines whether current buffer's `major-mode' is
`beancount-mode', and the cursor is at the beginning of the
line."
  (when (derived-mode-p 'beancount-mode)
    (not (or (nth 3 (syntax-ppss))
             (nth 4 (syntax-ppss))))))

(defun +rime--english-prober()
  "Using different probe lists in different modes."
  (if (derived-mode-p 'telega-chat-mode
                      'text-mode)
      (rime--probe-auto-english)
    (or (rime--after-alphabet-char-p)
        (rime--prog-in-code-p)
        (+rime--beancount-p))))

(setq rime-disable-predicates '((lambda () (button-at (point)))
                                rime--evil-mode-p
                                rime--punctuation-line-begin-p
                                +rime--english-prober))
