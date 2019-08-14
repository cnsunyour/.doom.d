;;; ~/.doom.d/+chinese.el -*- lexical-binding: t; -*-

;; 中文字体包
;; (def-package! cnfonts
;;   :config
;;   (cnfonts-enable)
;;   (setq cnfonts-use-face-font-rescale t))

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
              :slant 'normal)))


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
  (cl-destructuring-bind (paragraph content info) args
    (let* ((fix-regexp "[[:multibyte:]a-zA-Z0-9]")
           (origin-contents content)
           (fixed-contents
            (replace-regexp-in-string
             (concat "\\("
                     fix-regexp
                     "\\) *\\(<[Bb][Rr] */>\\)?\n *\\("
                     fix-regexp
                     "\\)")
             "\\1\\3"
             origin-contents)))
      (list paragraph fixed-contents info))))



(use-package! pyim
  :after-call after-find-file pre-command-hook
  :config
  (setq pyim-dcache-directory (concat doom-cache-dir "pyim/")
        pyim-page-tooltip t
        default-input-method "pyim"
        pyim-default-scheme 'wubi
        pyim-page-tooltip 'posframe))

(map! :map 'pyim-mode-map
      ";" (lambda ()
            (interactive)
            (pyim-page-select-word-by-number 2))
      "'" (lambda ()
            (interactive)
            (pyim-page-select-word-by-number 3)))
