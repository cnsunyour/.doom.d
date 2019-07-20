;;; ~/.doom.d/+chinese.el -*- lexical-binding: t; -*-

;; 中文字体包
;; (def-package! cnfonts
;;   :config
;;   (cnfonts-enable)
;;   (setq cnfonts-use-face-font-rescale t)
;;   )

;; 中文字体配置
(defun cnsunyour/better-font()
  (interactive)
  (when (display-graphic-p)
    (progn
      (set-face-attribute
       'default nil
       :font (font-spec :name "-*-PragmataPro-normal-normal-normal-*-*-*-*-*-p-0-iso10646-1"
                        :weight 'normal
                        :slant 'normal
                        :size 14.0))
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font
         (frame-parameter nil 'font)
         charset
         (font-spec :name "-*-Microsoft YaHei-normal-normal-normal-*-*-*-*-*-p-0-iso10646-1"
                    :weight 'normal
                    :slant 'normal
                    :size 14.5)))
      )))
(defun cnsunyour/init-font(frame)
  (with-selected-frame frame
    (when (display-graphic-p)
      (cnsunyour/better-font))))
(if (and (fboundp 'daemonp) (daemonp))
    (add-hook 'after-make-frame-functions #'cnsunyour/init-font)
  (cnsunyour/better-font))

;; 取消pangu-spacing包里默认的在中英文字符之间增加空格的设置
;; (global-pangu-spacing-mode 0)
;; (set (make-local-variable 'pangu-spacing-real-insert-separtor) nil)

;; 修改pyim默认输入法为五笔，使用posframe能使输入法tooltip显示更顺畅
(setq pyim-default-scheme 'wubi)
(setq pyim-page-tooltip 'posframe)
