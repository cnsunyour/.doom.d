;;; cnsunyour/chinese/+rime-probe-english.el -*- lexical-binding: t; -*-

(defun +rime--probe-dynamic-english ()
  "判断当前光标位置前一个字符是否为英文、数字或字符。"
  (looking-back "[a-zA-Z][0-9\x21-\x2f\x3a-\x40\x5b-\x60\x7b-\x7f]*" 1))

(defun +rime--probe-auto-english ()
  "激活这个探针函数后，使用下面的规则自动切换中英文输入：

1. 当前字符为英文字符（不包括空格）时，输入下一个字符为英文字符
2. 当前字符为中文字符或输入字符为行首字符时，输入的字符为中文字符
3. 以单个空格为界，自动切换中文和英文字符
   即，形如 `我使用 emacs 编辑此函数' 的句子全程自动切换中英输入法
"
  (if (> (point) (save-excursion (back-to-indentation) (point)))
      (if (looking-back " +" 1)
          (looking-back "\\cc +" 2)
        (not (looking-back "\\cc" 1)))))

(defun +rime--beancount-p ()
  "当前为`beancount-mode'，且光标在注释或字符串当中。"
  (when (derived-mode-p 'beancount-mode)
    (not (or (nth 3 (syntax-ppss))
             (nth 4 (syntax-ppss))))))

(defun +rime--evil-mode-p ()
  "检测当前是否在 `evil' 模式下。"
  (or (evil-normal-state-p)
      (evil-visual-state-p)
      (evil-motion-state-p)
      (evil-operator-state-p)))

(defun +rime--english-prober()
  "自定义英文输入探针函数，用于在不同mode下使用不同的探针列表"
  (if (derived-mode-p 'telega-chat-mode
                      'text-mode)
      (+rime--probe-auto-english)
    (or (+rime--probe-dynamic-english)
        (rime--prog-in-code-p)
        (+rime--beancount-p))))


(setq rime-disable-predicates '((lambda () (button-at (point)))
                                +rime--evil-mode-p
                                +rime--english-prober))
