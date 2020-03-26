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
              '(rime-predicate-current-uppercase-letter-p))

(add-hook! (telega-chat-mode text-mode)
  (setq-local rime-disable-predicates
              '(+rime-predicate-button-at-point-p
                rime-predicate-evil-mode-p
                +rime-predicate-after-special-punctuation-p
                rime-predicate-punctuation-line-begin-p
                rime-predicate-punctuation-after-space-cc-p
                rime-predicate-punctuation-after-ascii-p))
  (setq-local rime-inline-predicates
              '(rime-predicate-current-uppercase-letter-p
                rime-predicate-auto-english-p)))

;;; +rime-predicates.el ends here
