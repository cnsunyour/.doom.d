;;; +rime-predicates.el --- Auto switch between Chinese and English input state.
;;; cnsunyour/chinese/+rime-predicates.el -*- lexical-binding: t; -*-


;;; Commentary:
;;
;; Some personal custom predicates for `emacs-rime'.
;;
;; With this configuraction, I can continuously input mixed Chinese and English
;; text with punctuation, only using the SpaceBar and the Enter key to assist,
;; without the extra switch key.
;;

;;; Code:

(defun +rime-predicate-current-input-uppercase-letter-p ()
  "If the current charactor entered is a uppercase letter.

Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and rime--current-input-key
     (>= rime--current-input-key ?A)
     (<= rime--current-input-key ?Z)))

(defun +rime-predicate-after-ascii-char-p ()
  "If the cursor is after a ascii character.

Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and (> (point) (save-excursion (back-to-indentation) (point)))
       (looking-back "[a-zA-Z0-9\x21-\x2f\x3a-\x40\x5b-\x60\x7b-\x7f]" 1)))

(defun +rime-predicate-auto-english-in-evil-insert-p ()
  "Auto switch english in `evil-insert-state'.

由于 doom-emacs 在 `evil-insert-state' 把 RET 重新映射到了
`org-return-indent'，不知为何会导致 `emacs-rime' 在 inline_ascii
模式无法通过按 RET 完成上屏，因此此断言用在 `rime-disable-predicate'
里，用于切换成普通英文模式，注意不要把此断言用于
`rime-inline-predicate' 中。"
  (and (fboundp 'evil-mode)
       (evil-insert-state-p)
       (or (+rime-predicate-current-input-uppercase-letter-p)
           (rime-predicate-auto-english-p))))

(defun +rime-predicate-current-input-punctuation-p ()
  "If the current charactor entered is a punctuation.

Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and rime--current-input-key
       (or (and (<= #x21 rime--current-input-key) (<= rime--current-input-key #x2f))
           (and (<= #x3a rime--current-input-key) (<= rime--current-input-key #x40))
           (and (<= #x5b rime--current-input-key) (<= rime--current-input-key #x60))
           (and (<= #x7b rime--current-input-key) (<= rime--current-input-key #x7f)))))

(defun +rime-predicate-punctuation-after-space-cc-p ()
  "If input a punctuation after a Chinese charactor with whitespace.

Can be used in `rime-disable-predicates' and `rime-inline-predicates'.\""
  (and (> (point) (save-excursion (back-to-indentation) (point)))
       (+rime-predicate-current-input-punctuation-p)
       (looking-back "\\cc +" 2)))

(defun +rime-predicate-punctuation-after-ascii-p ()
  "If input a punctuation after a ascii charactor with whitespace.

Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and (+rime-predicate-current-input-punctuation-p)
       (+rime-predicate-after-ascii-char-p)))

(defun +rime-predicate-after-special-punctuation-p ()
  "If the cursor is after a string prefixed a special punctuation.

Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and (> (point) (save-excursion (back-to-indentation) (point)))
       (looking-back "[@:/][a-zA-Z0-9-_]*" 1)))

(defun +rime-predicate-button-at-point-p ()
  "Determines whether the point is a button.

\"Button\" means that positon is not editable.

Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (button-at (point)))

(defun +rime-predicate-beancount-p ()
  "Predicate input state in `beancount-mode'.

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
(setq-default rime-inline-predicates
              '(+rime-predicate-current-input-uppercase-letter-p))

(add-hook! (telega-chat-mode text-mode)
  (setq-local rime-disable-predicates
              '(+rime-predicate-button-at-point-p
                rime-predicate-evil-mode-p
                +rime-predicate-auto-english-in-evil-insert-p
                +rime-predicate-after-special-punctuation-p
                rime-predicate-punctuation-line-begin-p
                +rime-predicate-punctuation-after-space-cc-p
                +rime-predicate-punctuation-after-ascii-p))
  (setq-local rime-inline-predicates
              '(+rime-predicate-current-input-uppercase-letter-p
                rime-predicate-auto-english-p)))

;;; +rime-predicates.el ends here
