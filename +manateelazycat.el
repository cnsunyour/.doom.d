;;; ~/.doom.d/+manateelazycat.el -*- lexical-binding: t; -*-

;; 英文自动补全和翻译，激活命令toggle-company-english-helper
(use-package! company-english-helper
  :defer t
  :commands toggle-company-english-helper
  :init
  (map! :leader :g "yM" #'toggle-company-english-helper))

;; 输入insert-translated-name-insert激活命令，可以输入中文后按空格翻译成英文插入当前位置。
(use-package! insert-translated-name
  :defer t
  :commands insert-translated-name-insert-original-translation
  :init
  (map! :leader :g "ym" #'insert-translated-name-insert-original-translation))

;; sdcv翻译当前单词
(use-package! sdcv
  :defer t
  :commands sdcv-search-pointer+ sdcv-search-pointer
  :init
  (map! :leader
        :g "yd" #'sdcv-search-pointer+
        :g "yD" #'sdcv-search-pointer)
  :config
  (set-popup-rule! "^\\*SDCV\\*" :side 'right :size 0.4 :select t)
  (set-face-background 'sdcv-tooltip-face nil)
  (set-face-foreground 'sdcv-tooltip-face nil)
  (setq sdcv-dictionary-data-dir (expand-file-name "~/.stardict/dic"))
  (setq sdcv-dictionary-simple-list     ;setup dictionary list for simple search
        '("懒虫简明英汉词典"
          "懒虫简明汉英词典"
          "朗道英汉字典5.0"
          "朗道汉英字典5.0"))
  (setq sdcv-dictionary-complete-list   ;setup dictionary list for complete search
        '("懒虫简明英汉词典"
          "懒虫简明汉英词典"
          "朗道英汉字典5.0"
          "朗道汉英字典5.0"
          ;; "KDic11万英汉词典"
          ;; "XDICT英汉辞典"
          ;; "XDICT汉英辞典"
          "21世纪英汉汉英双向词典"
          ;; "21世纪双语科技词典"
          "牛津英汉双解美化版"
          ;; "英汉汉英专业词典"
          ;; "新世纪英汉科技大词典"
          ;; "新世纪汉英科技大词典"
          "现代汉语词典"
          "高级汉语大词典")))

(use-package! awesome-tab
  :commands (awesome-tab-mode)
  :init
  (defhydra hydra-tab (:pre (awesome-tab-mode t)
                       :post (awesome-tab-mode -1))
    "
   ^^^^Fast Move             ^^^^Tab                    ^^Search            ^^Misc
  -^^^^--------------------+-^^^^---------------------+-^^----------------+-^^---------------------------
     ^_k_^   prev group    | _C-a_^^     select first | _b_ search buffer | _C-k_   kill buffer
   _h_   _l_ switch tab    | _C-e_^^     select last  | _g_ search group  | _C-S-k_ kill others in group
     ^_j_^   next group    | _a_^^       ace jump     | ^^                | ^^
   ^^0 ~ 9^^ select window | _C-h_/_C-l_ move current | ^^                | ^^
  -^^^^--------------------+-^^^^---------------------+-^^----------------+-^^---------------------------
  "
    ("h" awesome-tab-backward-tab)
    ("j" awesome-tab-forward-group)
    ("k" awesome-tab-backward-group)
    ("l" awesome-tab-forward-tab)
    ("a" awesome-tab-ace-jump)
    ("C-a" awesome-tab-select-beg-tab)
    ("C-e" awesome-tab-select-end-tab)
    ("C-h" awesome-tab-move-current-tab-to-left)
    ("C-l" awesome-tab-move-current-tab-to-right)
    ("b" ivy-switch-buffer)
    ("g" awesome-tab-counsel-switch-group)
    ("C-k" kill-current-buffer)
    ("C-S-k" awesome-tab-kill-other-buffers-in-current-group)
    ("q" nil "quit"))
  :bind
  (("s-t" . hydra-tab/body)))

(use-package! aweshell
  :defer t
  :init
  (map! :g "s-'" #'cnsunyour/call-aweshell-new)
  :config
  (defun cnsunyour/call-aweshell-new ()
    (interactive)
    (progn
      (aweshell-new)
      (delete-other-windows))))

;; web-mode下标签改名和高亮插件
(use-package! instant-rename-tag :defer t)
(use-package! highlight-matching-tag
  :defer t
  :config
  (highlight-matching-tag 1))

;; A modern, easy-to-expand fuzzy search framework
;; M-x snails or M-x snails-search-point
(use-package! snails
  :load-path "/Users/yaohui/repos/snails"
  :defer t
  :commands snails snails-search-point
  :init
  (map! :leader
        (:g "sn" #'snails)
        (:g "sN" #'snails-search-point))
  :hook
  ('snails-mode . #'evil-emacs-state)
  :config
  (add-to-list 'snails-default-backends #'snails-backend-current-buffer t))
