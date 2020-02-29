;;; ~/.doom.d/+chinese.el -*- lexical-binding: t; -*-

;; 中文字体包
;; (use-package! cnfonts
;;   :config
;;   (cnfonts-enable)
;;   (setq cnfonts-use-face-font-rescale t))

;; (when (display-graphic-p)
;;   (set-face-attribute
;;    'default nil
;;    :font (font-spec :name "-*-PragmataPro-normal-normal-normal-*-*-*-*-*-p-0-iso10646-1"
;;                     :weight 'normal
;;                     :slant 'normal
;;                     :size 14.0))
;;   (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;     (set-fontset-font
;;      (frame-parameter nil 'font)
;;      charset
;;      (font-spec :name "-*-Microsoft YaHei-normal-normal-normal-*-*-*-*-*-p-0-iso10646-1"
;;                 :weight 'normal
;;                 :slant 'normal))))

(use-package! fcitx
  :after evil
  :config
  (when (executable-find "fcitx-remote")
    (fcitx-evil-turn-on)))

(use-package! ace-pinyin
  :after avy
  :init (setq ace-pinyin-use-avy t)
  :config (ace-pinyin-global-mode t))


;;
;;; Hack
;;;
(defadvice! +chinese--org-html-paragraph-a (args)
  "Join consecutive Chinese lines into a single long line without
unwanted space when exporting org-mode to html."
  :filter-args #'org-html-paragraph
  (++chinese--org-paragraph args))

(defadvice! +chinese--org-hugo-paragraph-a (args)
  "Join consecutive Chinese lines into a single long line without
unwanted space when exporting org-mode to hugo markdown."
  :filter-args #'org-hugo-paragraph
  (++chinese--org-paragraph args))

(defun ++chinese--org-paragraph (args)
  (cl-destructuring-bind (paragraph content info) args
    (let* ((origin-contents
            (replace-regexp-in-string
             "<[Bb][Rr][[:blank:]]*/>"
             ""
             content))
           (origin-contents
            (replace-regexp-in-string
             "\\([[:multibyte:]]\\)[[:blank:]]*\n[[:blank:]]*\\([[:multibyte:]]\\)"
             "\\1\\2"
             origin-contents))
           (fixed-contents
            (replace-regexp-in-string
             "\\([^[:blank:]]\\)[[:blank:]]*\n[[:blank:]]*\\([^[:blank:]]\\)"
             "\\1 \\2"
             origin-contents)))
      (list paragraph fixed-contents info))))


(use-package! liberime-config
  :load-path "~/repos/liberime"
  :init
  (setq liberime-shared-data-dir (file-truename "~/Library/Rime")
        liberime-user-data-dir (file-truename "~/.local/pyim/rime"))
  :hook
  ('after-liberime-load . (lambda ()
                            (liberime-select-schema "wubi86"))))

(use-package! pyim
  :after liberime-config
  :after-call after-find-file pre-command-hook
  :init
  (setq pyim-title "ㄓ")
  :bind
  ("C-S-s-j" . pyim-convert-string-at-point) ;与 pyim-probe-dynamic-english 配合
  :config
  (setq pyim-dcache-directory (expand-file-name "~/.local/pyim/cache/")
        default-input-method "pyim"
        pyim-default-scheme 'rime
        pyim-assistant-scheme 'quanpin
        pyim-page-tooltip 'posframe
        pyim-page-length 5)

  (defun cnsunyour/pyim-english-prober()
    "自定义英文输入探针函数，用于在不同mode下使用不同的探针列表"
    (let ((use-en (button-at (point))))
      (if (derived-mode-p 'telega-chat-mode)
          (setq use-en (or use-en
                           (pyim-probe-auto-english)))
        (when (derived-mode-p 'text-mode)
          (setq use-en (or use-en
                           (pyim-probe-auto-english))))
        (when (or (derived-mode-p 'prog-mode)
                  (derived-mode-p 'conf-mode))
          (setq use-en (or use-en
                           (pyim-probe-dynamic-english))))
        (unless (derived-mode-p 'beancount-mode)
          (setq use-en (or use-en
                           (pyim-probe-program-mode)
                           (pyim-probe-org-speed-commands)
                           (pyim-probe-org-structure-template)))))
      use-en))
  ;; 设置英文输入探针方式，采用自定义探针函数
  (setq-default pyim-english-input-switch-functions
                '(cnsunyour/pyim-english-prober))

  (defun cnsunyour/pyim-punctuation-prober(char)
    "自定义标点符号半角探针函数，用于在不同mode下使用不同的探针列表"
    (or (pyim-probe-punctuation-line-beginning char)
        (pyim-probe-punctuation-after-punctuation char)))
  ;; 设置标点符号半角探针方式，采用自定义探针函数
  (setq-default pyim-punctuation-half-width-functions
                '(cnsunyour/pyim-punctuation-prober))

  (map! :map 'pyim-mode-map
        "." 'pyim-page-next-page
        "," 'pyim-page-previous-page
        ";" (λ! (pyim-page-select-word-by-number 2))
        "'" (λ! (pyim-page-select-word-by-number 3))))


;; Support pinyin in Ivy
;; Input prefix ';' to match pinyin
;; Refer to  https://github.com/abo-abo/swiper/issues/919 and
;; https://github.com/pengpengxp/swiper/wiki/ivy-support-chinese-pinyin
(use-package! pinyinlib
  :commands pinyinlib-build-regexp-string
  :init
  (with-no-warnings
    (defun ivy--regex-pinyin (str)
      "The regex builder wrapper to support pinyin."
      (or (pinyin-to-utf8 str)
          (and (fboundp '+ivy-prescient-non-fuzzy)
               (+ivy-prescient-non-fuzzy str))
          (ivy--regex-plus str)))

    (defun my-pinyinlib-build-regexp-string (str)
      "Build a pinyin regexp sequence from STR."
      (cond ((equal str ".*") ".*")
            (t (pinyinlib-build-regexp-string str t))))

    (defun my-pinyin-regexp-helper (str)
      "Construct pinyin regexp for STR."
      (cond ((equal str " ") ".*")
            ((equal str "") nil)
            (t str)))

    (defun pinyin-to-utf8 (str)
      "Convert STR to UTF-8."
      (cond ((equal 0 (length str)) nil)
            ((equal (substring str 0 1) ";")
             (mapconcat
              #'my-pinyinlib-build-regexp-string
              (remove nil (mapcar
                           #'my-pinyin-regexp-helper
                           (split-string
                            (replace-regexp-in-string ";" "" str)
                            "")))
              ""))
            (t nil)))

    (mapcar
     (lambda (item)
       (let ((key (car item))
             (value (cdr item)))
         (when (member value '(+ivy-prescient-non-fuzzy
                               ivy--regex-plus))
           (setf (alist-get key ivy-re-builders-alist)
                 #'ivy--regex-pinyin))))
     ivy-re-builders-alist)))
