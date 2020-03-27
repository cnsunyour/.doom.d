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
       (let ((string (buffer-substring (point) (line-beginning-position))))
         (string-match "[@:][a-zA-Z0-9-_]*$" string))))

;;;###overwrite
(defun rime-predicate-after-alphabet-char-p ()
  "If the cursor is after a alphabet character.

Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and (> (point) (save-excursion (back-to-indentation) (point)))
       (let ((string (buffer-substring (point) (line-beginning-position))))
         (string-match "[a-zA-Z][0-9\x21-\x2f\x3a-\x40\x5b-\x60\x7b-\x7f]*$" string))))

;;;###overwrite
(defun rime-predicate-after-ascii-char-p ()
  "If the cursor is after a ascii character.

Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and (> (point) (save-excursion (back-to-indentation) (point)))
       (let ((string (buffer-substring (point) (line-beginning-position))))
         (string-match "[a-zA-Z0-9\x21-\x2f\x3a-\x40\x5b-\x60\x7b-\x7f]$" string))))

;;;###overwrite
(defun rime-predicate-punctuation-after-space-cc-p ()
  "If input a punctuation after a Chinese charactor with whitespace.

Can be used in `rime-disable-predicates' and `rime-inline-predicates'.\""
  (and (> (point) (save-excursion (back-to-indentation) (point)))
       (rime-predicate-current-input-punctuation-p)
       (let ((string (buffer-substring (point) (line-beginning-position))))
         (string-match "\\cc +$" string))))

;;;###overwrite
(defun rime-predicate-auto-english-p ()
  "Auto switch Chinese/English input state.

  After activating this probe function, use the following rules
  to automatically switch between Chinese and English input:

     1. When the current character is an English
  character (excluding spaces), enter the next character as an
  English character.
    2. When the current character is a Chinese character or the
  input character is a beginning character, the input character is
  a Chinese character.
     3. With a single space as the boundary, automatically switch
  between Chinese and English characters.

  That is, a sentence of the form \"我使用 emacs 编辑此函数\"
  automatically switches between Chinese and English input methods.

  Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and (> (point) (save-excursion (back-to-indentation) (point)))
       (let ((string (buffer-substring (point) (line-beginning-position))))
         (if (string-match " +$" string)
             (string-match "\\cc +$" string)
           (not (string-match "\\cc$" string))))))

;;;###overwrite
(defun rime-predicate-space-after-ascii-p ()
  "If cursor is after a whitespace which follow a ascii character."
  (and (> (point) (save-excursion (back-to-indentation) (point)))
       (let ((string (buffer-substring (point) (line-beginning-position))))
         (and (string-match " +$" string)
              (not (string-match "\\cc +$" string))))))

;;;###overwrite
(defun rime-predicate-space-after-cc-p ()
  "If cursor is after a whitespace which follow a non-ascii character."
  (and (> (point) (save-excursion (back-to-indentation) (point)))
       (let ((string (buffer-substring (point) (line-beginning-position))))
         (string-match "\\cc +$" string))))

(defun rime-predicate-button-at-point-p ()
  "Detect whether the point is a button.

\"Button\" means that positon is not editable.

Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (button-at (point)))

(defun rime-predicate-ace-window-mode-p ()
  "Detect if the `ace-window-mode' is enabled.

Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and (boundp 'ace-window-mode)
       ace-window-mode))

(defun +rime-predicate-beancount-p ()
  "Predicate input state in `beancount-mode'.

Detect whether current buffer's `major-mode' is `beancount-mode',
and the cursor is in the comments or strings.

Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (when (derived-mode-p 'beancount-mode)
    (not (or (nth 3 (syntax-ppss))
             (nth 4 (syntax-ppss))))))

(setq-default rime-disable-predicates
              '(rime-predicate-button-at-point-p
                rime-predicate-evil-mode-p
                rime-predicate-ace-window-mode-p
                rime-predicate-punctuation-line-begin-p
                rime-predicate-after-alphabet-char-p
                rime-predicate-prog-in-code-p
                +rime-predicate-beancount-p))
(setq-default rime-inline-predicates
              '(rime-predicate-current-uppercase-letter-p))

(add-hook! (telega-chat-mode text-mode)
  (setq-local rime-disable-predicates
              '(rime-predicate-button-at-point-p
                rime-predicate-evil-mode-p
                rime-predicate-ace-window-mode-p
                +rime-predicate-after-special-punctuation-p
                rime-predicate-punctuation-line-begin-p
                rime-predicate-punctuation-after-space-cc-p
                rime-predicate-punctuation-after-ascii-p))
  (setq-local rime-inline-predicates
              '(rime-predicate-current-uppercase-letter-p
                rime-predicate-auto-english-p)))

;;; +rime-predicates.el ends here
