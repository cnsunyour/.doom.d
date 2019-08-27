;;; ~/.doom.d/+translate.el -*- lexical-binding: t; -*-

;; 有道词典
(use-package! youdao-dictionary
  :defer t
  :init
  (map! (:leader
          :gnv "yy" #'youdao-dictionary-search-at-point++
          :gnv "yY" #'youdao-dictionary-search-at-point
          :gnv "yp" #'youdao-dictionary-play-voice-at-point
          :gnv "yP" #'youdao-dictionary-play-voice-from-input))
  :config
  (setq youdao-dictionary-api-app-key "702073188172eb7d")
  (setq youdao-dictionary-api-app-secret "hORHRHI3pXFl95ARploOVQHHZrr5mcBp")
  ;; Enable Cache
  (setq url-automatic-caching t)
  ;; Enable Chinese word segmentation support (支持中文分词)
  (setq youdao-dictionary-use-chinese-word-segmentation t))


;; Google translate
;; 让google-translate使用posframe显示弹出窗口，比popup显示更好看，没有对齐问题
;; Let google-translate use posframe to display tooltip, better than popup,
;; no alignment problem
(require 'google-translate)

(setq google-translate-base-url
      "http://translate.google.cn/translate_a/single")
(setq google-translate-listen-url
      "http://translate.google.cn/translate_tts")
(setq google-translate-backend-method 'curl)
(setq google-translate-default-source-language "en")
(setq google-translate-default-target-language "zh-CN")

(defcustom google-translate-tooltip-name "*google-translate-posframe*"
  "The name of google translate tooltip name."
  :type 'string
  :group 'google-translate)

(defvar google-translate-tooltip-last-point 0
  "Hold last point when show tooltip, use for hide tooltip after move point.")

(defvar google-translate-tooltip-last-scroll-offset 0
  "Hold last scroll offset when show tooltip, use for hide tooltip after window scroll.")

(defun google-translate-hide-tooltip-after-move ()
  (ignore-errors
    (when (get-buffer google-translate-tooltip-name)
      (unless (and
               (equal (point) google-translate-tooltip-last-point)
               (equal (window-start) google-translate-tooltip-last-scroll-offset))
        (posframe-delete google-translate-tooltip-name)
        (kill-buffer google-translate-tooltip-name)))))

(defun google-translate-show-posframe-tooltip (text)
  "Show string on posframe buffer."
  ;; Show tooltip at point if word fetch from user cursor.
  (require 'posframe)
  (posframe-show google-translate-tooltip-name
                 :string text
                 :position (point)
                 :timeout 8
                 :internal-border-width 10)
  (add-hook 'post-command-hook 'google-translate-hide-tooltip-after-move)
  (setq google-translate-tooltip-last-point (point))
  (setq google-translate-tooltip-last-scroll-offset (window-start)))

(defun -region-or-word ()
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (thing-at-point 'word t)))

(defun -chinese-word-p (word)
  (if (and word (string-match "\\cc" word)) t nil))

(defun -translate-request (source-language target-language text)
  (let* ((json (google-translate-request source-language
                                         target-language
                                         text)))
    (if (null json)
        (message "Nothing to translate.")
      (let* ((detailed-translation
              (google-translate-json-detailed-translation json))
             (detailed-definition
              (google-translate-json-detailed-definition json))
             (gtos
              (make-gtos
               :source-language source-language
               :target-language target-language
               :auto-detected-language (aref json 2)
               :text text
               :text-phonetic (google-translate-json-text-phonetic json)
               :translation (google-translate-json-translation json)
               :translation-phonetic (google-translate-json-translation-phonetic json)
               :detailed-translation detailed-translation
               :detailed-definition detailed-definition
               :suggestion (when (null detailed-translation)
                             (google-translate-json-suggestion json)))))
        (google-translate-posframe-output-translation gtos)))))

(defun google-translate-posframe-output-translation (gtos)
  "Output translation to the popup tooltip using `popup' package."
  (google-translate-show-posframe-tooltip
   (with-temp-buffer
     (google-translate-buffer-insert-translation gtos)
     (google-translate--trim-string
      (buffer-substring (point-min) (point-max))))))

(defun %google-translate-at-point++ (override-p reverse-p)
  (let* ((langs (google-translate-read-args override-p reverse-p))
         (source-language (car langs))
         (target-language (cadr langs))
         (bounds nil))
    (-translate-request
     source-language target-language
     (if (use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end))
       (or (and (setq bounds (bounds-of-thing-at-point 'word))
                (buffer-substring-no-properties (car bounds) (cdr bounds)))
           (error "No word at point."))))))

(defun google-translate-at-point++ (&optional override-p)
  "Translate at point and show result with posframe."
  (interactive "P")
  (%google-translate-at-point++ override-p nil))

(defun google-translate-at-point-reverse++ (&optional override-p)
  "Translate reverse at point and show result with posframe."
  (interactive "P")
  (%google-translate-at-point++ override-p t))

(defun google-translate-chinese-at-point++ (&optional override-p)
  "如果当前位置是中文，则自动调用反向进行中转英翻译，否则进行正向
英转中翻译。并在posframe提示框里显示结果。此方法只能用于点词翻译，
不能用于划词翻译，至于原因，我还没弄明白。"
  (interactive "P")
  (if (-chinese-word-p(-region-or-word))
      (%google-translate-at-point++ override-p t)
    (%google-translate-at-point++ override-p nil)))

(defun google-translate-chinese-at-point (&optional override-p)
  "如果当前位置是中文，则自动调用反向进行中转英翻译，否则进行正向
英转中翻译。并在另一个buffer里显示结果。此方法既可用于点词翻译，
也可用于划词翻译。"
  (interactive "P")
  (if (-chinese-word-p(-region-or-word))
      (%google-translate-at-point override-p t)
    (%google-translate-at-point override-p nil)))

(map! (:leader
        :gnv "yt" #'google-translate-chinese-at-point++
        :gnv "yT" #'google-translate-chinese-at-point))
