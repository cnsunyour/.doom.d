;;; ~/.doom.d/+chinese.el -*- lexical-binding: t; -*-


;; (use-package! pangu-spacing
;;   :hook (text-mode . pangu-spacing-mode)
;;   :config
;;   ;; Always insert `real' space in org-mode.
;;   (setq-hook! 'org-mode-hook pangu-spacing-real-insert-separtor t))

;; (use-package! fcitx
;;   :config
;;   (when (fcitx-check-status)
;;     (fcitx-prefix-keys-setup)
;;     (fcitx-prefix-keys-turn-on)
;;     (fcitx-M-x-turn-on)
;;     (fcitx-shell-command-turn-on)
;;     (fcitx-eval-expression-turn-on)
;;     (fcitx-read-funcs-turn-on)
;;     (fcitx-evil-turn-on)
;;     (fcitx-aggressive-minibuffer-turn-on)
;;     (fcitx-org-speed-command-turn-on)
;;     (fcitx-isearch-turn-on)))

(use-package! ace-pinyin
  :after avy
  :init (setq ace-pinyin-use-avy t)
  :config (ace-pinyin-global-mode t))


;;
;;; Hack
;;;
(define-advice org-html-paragraph (:filter-args (args) chinese-a)
  "Join consecutive Chinese lines into a single long line without
unwanted space when exporting org-mode to html."
  (++chinese--org-paragraph args))

(define-advice org-hugo-paragraph (:filter-args (args) chinese-a)
  "Join consecutive Chinese lines into a single long line without
unwanted space when exporting org-mode to hugo markdown."
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


(use-package! rime
  :bind
  ("C-s-S-j" . #'+rime-convert-string-at-point)
  (:map rime-active-mode-map
   ("C-s-S-j" . #'rime-inline-ascii)
   ("C-M-S-s-j" . #'rime-inline-ascii))
  (:map rime-mode-map
   ("C-M-S-s-j" . #'rime-force-enable)
   ("C-." . #'rime-send-keybinding)
   ("S-SPC" . #'rime-send-keybinding)
   ("S-<delete>" . #'rime-send-keybinding)
   ("C-`" . #'rime-send-keybinding)
   ("C-~" . #'rime-send-keybinding)
   ("C-S-`" . #'rime-send-keybinding))
  :config
  (setq default-input-method "rime"
        rime-user-data-dir (expand-file-name "~/.local/emacs-rime")
        rime-show-candidate 'posframe
        rime-posframe-style 'simple
        rime-popup-style 'simple
        rime-sidewindow-style 'simple
        rime-inline-ascii-trigger 'shift-l)

  (add-hook! (org-mode
              markdown-mode
              beancount-mode)
    (activate-input-method default-input-method))

  (defun +rime-force-enable ()
    "[ENHANCED] Force into Chinese input state.

If current input method is not `rime', active it first. If it is
currently in the `evil' non-editable state, then switch to
`evil-insert-state'."
    (interactive)
    (let ((input-method "rime"))
      (unless (string= current-input-method input-method)
        (activate-input-method input-method))
      (when (rime-predicate-evil-mode-p)
        (if (= (1+ (point)) (line-end-position))
            (evil-append 1)
          (evil-insert 1)))
      (rime-force-enable)))

  (defun +rime-convert-string-at-point ()
    "Convert the string at point to Chinese using the current input scheme.

First call `+rime-force-enable' to active the input method, and
then search back from the current cursor for available string (if
a string is selected, use it) as the input code, call the current
input scheme to convert to Chinese."
    (interactive)
    (+rime-force-enable)
    (let ((string (if mark-active
                      (buffer-substring-no-properties
                       (region-beginning) (region-end))
                    (buffer-substring-no-properties
                     (point) (max (line-beginning-position) (- (point) 80)))))
          code
          length)
      (cond ((string-match "\\([a-z]+\\|[[:punct:]]\\)[[:blank:]]*$" string)
             (setq code (replace-regexp-in-string
                         "^[-']" ""
                         (match-string 0 string)))
             (setq length (length code))
             (setq code (replace-regexp-in-string " +" "" code))
             (if mark-active
                 (delete-region (region-beginning) (region-end))
               (when (> length 0)
                 (delete-char (- 0 length))))
             (when (> length 0)
               (setq unread-command-events
                     (append (listify-key-sequence code)
                             unread-command-events))))
            (t (message "`+rime-convert-string-at-point' did nothing.")))))

  (load! "+rime-predicates"))


;; let `ivy-read' support chinese pinyin
;; Input prefix ';' to match pinyin
;; Refer to  https://github.com/abo-abo/swiper/issues/919 and
;; https://github.com/pengpengxp/swiper/wiki/ivy-support-chinese-pinyin
(use-package! pinyinlib
  :when (modulep! :completion ivy)
  :config
  (defun re-builder-pinyin (str)
    (or (pinyin-to-utf8 str)
        (ivy--regex-plus str)
        (ivy--regex-ignore-order)))

  (setq ivy-re-builders-alist
        '((t . re-builder-pinyin)))

  (defun my-pinyinlib-build-regexp-string (str)
    (cond ((equal str ".*") ".*")
          (t (pinyinlib-build-regexp-string str t))))

  (defun my-pinyin-regexp-helper (str)
    (cond ((equal str " ") ".*")
          ((equal str "") nil)
          (t str)))

  (defun pinyin-to-utf8 (str)
    (cond ((equal 0 (length str)) nil)
          ((equal (substring str 0 1) ";")
           (mapconcat 'my-pinyinlib-build-regexp-string
                      (remove nil (mapcar 'my-pinyin-regexp-helper
                                          (split-string
                                           (replace-regexp-in-string ";" "" str)
                                           "")))
                      ""))
          nil)))


;; 基于 结巴分词 的 Emacs 中文分词 工具，实现了以中文词语为单位的移动和编辑。
;; +---------------+----------------------+--------------------------+
;; | key binding   | default command      | cns-mode command         |
;; +---------------+----------------------+--------------------------+
;; | M-b           | (backward-word)      | (cns-backward-word)      |
;; | M-f           | (forward-word)       | (cns-forward-word)       |
;; | C-<backspace> | (backward-kill-word) | (cns-backward-kill-word) |
;; | M-DEL         | (backward-kill-word) | (cns-backward-kill-word) |
;; | C-<delete>    | (kill-word)          | (cns-kill-word)          |
;; | M-d           | (kill-word)          | (cns-kill-word)          |
;; | M-t           | (transpose-words)    | (cns-transpose-words)    |
;; +---------------+----------------------+--------------------------+
(use-package! cns
  :hook (find-file . cns-auto-enable)
  :config
  (let ((cnsbasedir (expand-file-name
                     "cns"
                     (expand-file-name
                      straight-build-dir
                      (expand-file-name
                       "straight"
                       straight-base-dir)))))
    (setq cns-prog (expand-file-name "cnws" cnsbasedir)
          cns-dict-directory (expand-file-name "cppjieba/dict" cnsbasedir))
    (unless (file-exists-p cns-prog)
      (let ((default-directory cnsbasedir))
        (if (zerop (shell-command "make"))
            (message "cnws compiled successfully.")
          (error "cnws compile failed."))))))
