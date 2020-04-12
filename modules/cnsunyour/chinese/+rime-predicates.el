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

(defun +rime-predicates-basic ()
  "The basic necessary predicates combination."
  (or (rime-predicate-evil-mode-p)
      (rime-predicate-ace-window-p)
      (rime-predicate-hydra-p)
      (+rime-predicate-button-at-point-p)
      (rime-predicate-punctuation-line-begin-p)))

(defun +rime-predicate-button-at-point-p ()
  "Detect whether the point is a button.

\"Button\" means that positon is not editable.

Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (button-at (point)))

(defun +rime-predicate-after-special-punctuation-p ()
  "If the cursor is after a string prefixed a special punctuation.

Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and (> (point) (save-excursion (back-to-indentation) (point)))
       (let ((string (buffer-substring (point) (max (line-beginning-position) (- (point) 80)))))
         (string-match-p "[@:][a-zA-Z0-9-_]*$" string))))

(defun +rime-predicate-beancount-p ()
  "Predicate input state in `beancount-mode'.

Detect whether current buffer's `major-mode' is `beancount-mode',
and the cursor is in the comments or strings.

Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and (derived-mode-p 'beancount-mode)
       (not (or (nth 3 (syntax-ppss))
                (nth 4 (syntax-ppss))))))

(setq-default rime-disable-predicates
              '(+rime-predicates-basic
                rime-predicate-after-alphabet-char-p
                rime-predicate-prog-in-code-p
                +rime-predicate-beancount-p))
(setq-default rime-inline-predicates
              '(rime-predicate-current-uppercase-letter-p))

(add-hook! (text-mode)
  (setq-local rime-disable-predicates
              '(+rime-predicates-basic
                rime-predicate-org-in-src-block-p
                rime-predicate-org-latex-mode-p
                rime-predicate-punctuation-after-space-cc-p
                rime-predicate-punctuation-after-ascii-p))
  (setq-local rime-inline-predicates
              '(rime-predicate-current-uppercase-letter-p
                rime-predicate-space-after-cc-p
                rime-predicate-after-ascii-char-p)))

(add-hook! (telega-chat-mode)
  (setq-local rime-disable-predicates
              '(+rime-predicates-basic
                +rime-predicate-after-special-punctuation-p
                rime-predicate-punctuation-after-space-cc-p
                rime-predicate-punctuation-after-ascii-p))
  (setq-local rime-inline-predicates
              '(rime-predicate-current-uppercase-letter-p
                rime-predicate-space-after-cc-p
                rime-predicate-after-ascii-char-p)))

;;; +rime-predicates.el ends here
