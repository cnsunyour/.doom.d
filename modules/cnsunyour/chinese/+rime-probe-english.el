;;; cnsunyour/chinese/+rime-probe-english.el -*- lexical-binding: t; -*-

(defun +rime-predicate-button-at-point-p ()
  "Determines whether the point is a button.

\"Button\" means that positon is not editable."
  (button-at (point)))

(defun +rime-predicate-beancount-p ()
  "Predicate input state in `beancount-mode.'

Determines whether current buffer's `major-mode' is
`beancount-mode', and the cursor is at the beginning of the
line."
  (when (derived-mode-p 'beancount-mode)
    (not (or (nth 3 (syntax-ppss))
             (nth 4 (syntax-ppss))))))


(setq-default rime-disable-predicates
              '(+rime-predicate-button-at-point-p
                rime-predicate-evil-mode-p
                rime-predicate-punctuation-line-begin-p
                rime-predicate-after-alphabet-char-p
                rime-predicate-prog-in-code-p
                +rime-predicate-beancount-p))

(add-hook! '(telega-chat-mode text-mode)
  (lambda () (setq-local rime-disable-predicates
                    '(+rime-predicate-button-at-point-p
                      rime-predicate-evil-mode-p
                      rime-predicate-punctuation-line-begin-p
                      rime-predicate-auto-english-p))))
