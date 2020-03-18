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

;; (use-package! pangu-spacing
;;   :hook (text-mode . pangu-spacing-mode)
;;   :config
;;   ;; Always insert `real' space in org-mode.
;;   (setq-hook! 'org-mode-hook pangu-spacing-real-insert-separtor t))

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


;; (use-package! liberime-config
;;   :init
;;   (setenv "RIME_PATH" "~/repos/librime")
;;   (setq liberime-shared-data-dir (file-truename "~/Library/Rime")
;;         liberime-user-data-dir (file-truename "~/.local/liberime"))
;;   :hook
;;   ('after-init . (lambda ()
;;                    (when (fboundp 'liberime-sync-user-data)
;;                      (liberime-sync))))
;;   ('liberime-after-start . (lambda ()
;;                             (liberime-select-schema "wubi86_jidian"))))

(use-package! rime
  ;; :after liberime-config
  :after-call after-find-file pre-command-hook
  :bind
  ("C-S-s-j" . (lambda ()
                 (interactive)
                 (let ((input-method "rime"))
                   (unless (string= current-input-method input-method)
                     (activate-input-method input-method))
                   (rime-force-enable))))
  :custom
  (default-input-method "rime")
  (rime-librime-root "~/repos/librime/dist")
  (rime-share-data-dir (file-truename "~/Library/Rime"))
  (rime-user-data-dir (file-truename "~/.local/liberime"))
  (rime-show-candidate 'posframe)
  :config
  (defadvice! +rime--posframe-display-result-a (args)
    "给 `rime--posframe-display-result' 传入的字符串加一个全角空
格，以解决 `posframe' 偶尔吃字的问题。"
    :filter-args #'rime--posframe-display-result
    (cl-destructuring-bind (result) args
      (let ((newresult (if (string-blank-p result)
                           result
                         (concat result "　"))))
        (list newresult))))

  (load! "+rime-probe-english"))


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
