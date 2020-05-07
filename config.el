;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
(setq user-full-name "Sunn Yao"
      frame-title-format (concat "%b - " user-full-name "'s Emacs")
      user-mail-address "sunyour@gmail.com"
      epa-file-encrypt-to user-mail-address)

;; System locale to use for formatting time values.
;;
;; Make sure that the weekdays in the time stamps of your Org mode files and in
;; the agenda appear in English.
(setq system-time-locale "C")

;; 设置我所在地方的经纬度，calendar里有个功能是日月食的预测，和经纬度相联系的。
;; 让emacs能计算日出日落的时间，在 calendar 上用 S 即可看到
;; 另外根据日出日落时间切换主题也需要经纬度
(setq calendar-location-name "Beijing, China")
(setq calendar-latitude +39.9055472)
(setq calendar-longitude +116.3887056)

;; init ccls include path
(after! ccls
  (when IS-MAC
    (setq ccls-initialization-options
          `(:clang ,(list :extraArgs ["-isystem/Library/Developer/CommandLineTools/usr/include/c++/v1"
                                      "-isystem/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include"
                                      "-isystem/usr/local/include"]
                          :resourceDir (string-trim (shell-command-to-string "clang -print-resource-dir")))))))

;; define environmental variable for some works
(setenv "PKG_CONFIG_PATH"
        (replace-regexp-in-string
         (concat path-separator "+$")
         ""
         (concat
          "/usr/local/opt/libffi/lib/pkgconfig" path-separator
          "/usr/local/opt/qt/lib/pkgconfig" path-separator
          "/usr/local/opt/nss/lib/pkgconfig" path-separator
          "usr/local/opt/zlib/lib/pkgconfig" path-separator
          (getenv "PKG_CONFIG_PATH"))))

;; mac下使用系统废纸篓删除文件
(when IS-MAC
  (setq mac-system-move-file-to-trash-use-finder t
        delete-by-moving-to-trash t))

;; The idle delay in seconds until completion starts automatically.
;; (setq company-idle-delay 0)

;; Set default directory
(setq default-directory "~")

;;
;; Optimize garbage-collect
;; Related variable: `gc-cons-threshold'
;;
;; (defmacro k-time (&rest body)
;;   "Measure and return the time it takes evaluating BODY."
;;   `(let ((time (current-time)))
;;      ,@body
;;      (float-time (time-since time))))
;; ;; Execute `garbage-collect' when emacs is idle for a specified time
;; (defvar k-gc-timer
;;   (run-with-idle-timer 15 t
;;                        (lambda ()
;;                          (message "Garbage Collector has run for %.06fsec"
;;                                   (k-time (garbage-collect))))))
(defvar k-gc-timer
  (run-with-idle-timer 15 t #'garbage-collect))

;; cleanup trailing whitespaces before save buffers.
(add-hook! before-save
           #'delete-trailing-whitespace
           #'whitespace-cleanup)
