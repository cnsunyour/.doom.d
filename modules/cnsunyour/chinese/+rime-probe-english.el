;;; cnsunyour/chinese/+rime-probe-english.el -*- lexical-binding: t; -*-

(defun +rime-predicate-after-ascii-char-p ()
  "If the cursor is after a ascii character.

Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and (> (point) (save-excursion (back-to-indentation) (point)))
       (looking-back "[a-zA-Z0-9\x21-\x2f\x3a-\x40\x5b-\x60\x7b-\x7f]" 1)))

(defun +rime-predicate-current-input-punctuation-p ()
  "If the current charactor entered is a punctuation.

Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and rime--current-input-key
       (or (and (<= #x21 rime--current-input-key) (<= rime--current-input-key #x2f))
           (and (<= #x3a rime--current-input-key) (<= rime--current-input-key #x40))
           (and (<= #x5b rime--current-input-key) (<= rime--current-input-key #x60))
           (and (<= #x7b rime--current-input-key) (<= rime--current-input-key #x7f)))))

(defun +rime-predicate-puncutuation-after-space-cc-p ()
  "If input a punctuation after a Chinese charactor with whitespace.

Can be used in `rime-disable-predicates' and `rime-inline-predicates'.\""
  (and (> (point) (save-excursion (back-to-indentation) (point)))
       (+rime-predicate-current-input-punctuation-p)
       (looking-back "\\cc +" 2)))

(defun +rime-predicate-puncutuation-after-ascii-p ()
  "If input a punctuation after a ascii charactor with whitespace.

Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and (+rime-predicate-current-input-punctuation-p)
       (+rime-predicate-after-ascii-char-p)))

(defun +rime-predicate-button-at-point-p ()
  "Determines whether the point is a button.

\"Button\" means that positon is not editable.

Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (button-at (point)))

(defun +rime-predicate-beancount-p ()
  "Predicate input state in `beancount-mode.'

Determines whether current buffer's `major-mode' is
`beancount-mode', and the cursor is at the beginning of the
line.

Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
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

(add-hook! (telega-chat-mode text-mode)
  (setq-local rime-disable-predicates
              '(+rime-predicate-button-at-point-p
                rime-predicate-evil-mode-p
                rime-predicate-punctuation-line-begin-p
                +rime-predicate-puncutuation-after-space-cc-p
                +rime-predicate-puncutuation-after-ascii-p))
  (setq-local rime-inline-predicates
              '(rime-predicate-auto-english-p)))
