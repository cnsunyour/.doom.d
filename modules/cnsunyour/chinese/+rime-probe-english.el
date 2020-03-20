;;; cnsunyour/chinese/+rime-probe-english.el -*- lexical-binding: t; -*-

(defvar +rime--last-input-key 0
  "Saved last input key.")

(unless (fboundp 'rime-input-method)
  (error "Function `rime-input-method' is not available."))

(defadvice! +get-key--rime-input-method-a (key)
  "Get input key before `rime-input-method' is executed."
  :before #'rime-input-method
  (setq +rime--last-input-key key))

(defun +rime--punctuation-line-begin-p ()
  "Determines whether the current cursor is at the beginning of
the line and the character last inputed is symbol."
  (and (<= (point) (save-excursion (back-to-indentation) (point)))
       (or (and (<= #x21 +rime--last-input-key) (<= +rime--last-input-key #x2f))
           (and (<= #x3a +rime--last-input-key) (<= +rime--last-input-key #x40))
           (and (<= #x5b +rime--last-input-key) (<= +rime--last-input-key #x60))
           (and (<= #x7b +rime--last-input-key) (<= +rime--last-input-key #x7f)))))

(defun +rime--probe-dynamic-english ()
  "Determines whether the previous character at the current
cursor position is an english letter, number, or symbol."
  (looking-back "[a-zA-Z][0-9\x21-\x2f\x3a-\x40\x5b-\x60\x7b-\x7f]*" 1))

(defun +rime--probe-auto-english ()
  "After activating this probe function, use the following rules
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

Can be used in `rime-disable-predicates'."
  (if (> (point) (save-excursion (back-to-indentation) (point)))
      (if (looking-back " +" 1)
          (looking-back "\\cc +" 2)
        (not (looking-back "\\cc" 1)))))

(defun +rime--beancount-p ()
  "Determines whether current buffer's `major-mode' is
`beancount-mode', and the cursor is at the beginning of the
line."
  (when (derived-mode-p 'beancount-mode)
    (not (or (nth 3 (syntax-ppss))
             (nth 4 (syntax-ppss))))))

(defun +rime--evil-mode-p ()
  "determines whether the current buffer is in one of
`evil-normal-state' ,`evil-visual-state' , `evil-motion-state'
or`evil-operator-state'."
  (or (evil-normal-state-p)
      (evil-visual-state-p)
      (evil-motion-state-p)
      (evil-operator-state-p)))

(defun +rime--english-prober()
  "Using different probe lists in different modes."
  (if (derived-mode-p 'telega-chat-mode
                      'text-mode)
      (+rime--probe-auto-english)
    (or (+rime--probe-dynamic-english)
        (rime--prog-in-code-p)
        (+rime--beancount-p))))

(setq rime-disable-predicates '((lambda () (button-at (point)))
                                +rime--evil-mode-p
                                +rime--punctuation-line-begin-p
                                +rime--english-prober))
