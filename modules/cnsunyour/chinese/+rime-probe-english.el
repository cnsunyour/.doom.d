;;; cnsunyour/chinese/+rime-probe-english.el -*- lexical-binding: t; -*-


(require 'pyim-probe)

(defun pyim-char-before-to-string (num)
  "得到光标前第 `num' 个字符，并将其转换为字符串。"
  (let* ((point (point))
         (point-before (- point num)))
    (when (and (> point-before 0)
               (char-before point-before))
      (char-to-string (char-before point-before)))))

(defun pyim-char-after-to-string (num)
  "得到光标后第 `num' 个字符，并将其转换为字符串。"
  (let* ((point (point))
         (point-after (+ point num)))
    (when (char-after point-after)
      (char-to-string (char-after point-after)))))

(defun pyim-string-match-p (regexp string &optional start)
  "与 `string-match-p' 类似，如果 REGEXP 和 STRING 是非字符串时，
不会报错。"
  (and (stringp regexp)
       (stringp string)
       (string-match-p regexp string start)))

(defun pyim-entered-get (&optional type)
  "从 `pyim-entered-buffer' 中获取拼音字符串.
默认返回 entered buffer 中的全部字符串。如果 TYPE 取值为
point-before, 返回 entered buffer 中 point 之前的字符串，如果
TYPE 取值为 point-after, 返回 entered buffer 中 point 之后的字符
串。
这里替换为 nil"
  nil)

(defun cnsunyour/rime-english-prober()
  "自定义英文输入探针函数，用于在不同mode下使用不同的探针列表"
  (let ((use-en (or (button-at (point))
                    (evil-normal-state-p)
                    (evil-visual-state-p)
                    (evil-motion-state-p)
                    (evil-operator-state-p))))
    (if (derived-mode-p 'telega-chat-mode)
        (setq use-en (or use-en
                         (pyim-probe-auto-english)))
      (when (derived-mode-p 'text-mode)
        (setq use-en (or use-en
                         (pyim-probe-auto-english))))
      (when (derived-mode-p 'prog-mode 'conf-mode)
        (setq use-en (or use-en
                         (pyim-probe-dynamic-english))))
      (unless (derived-mode-p 'beancount-mode)
        (setq use-en (or use-en
                         (pyim-probe-program-mode)
                         (pyim-probe-org-speed-commands)
                         (pyim-probe-org-structure-template)))))
    use-en))


(setq rime-disable-predicates '(cnsunyour/rime-english-prober))
