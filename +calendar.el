;;; ~/.doom.d/+calendar.el -*- lexical-binding: t; -*-

;; 日历及纪念日相关设置

;; 设置我所在地方的经纬度，calendar里有个功能是日月食的预测，和你的经纬度相联系的。
;; 让emacs能计算日出日落的时间，在 calendar 上用 S 即可看到
(setq calendar-latitude +39.9055472)
(setq calendar-longitude +116.3887056)
(setq calendar-location-name "北京")

;; 定义可以设置农历纪念日的函数
(defun cnsunyour/diary-chinese-anniversary (lunar-month lunar-day &optional year mark)
  (if year
      (let* ((d-date (diary-make-date lunar-month lunar-day year))
             (a-date (calendar-absolute-from-gregorian d-date))
             (c-date (calendar-chinese-from-absolute a-date))
             (cycle (car c-date))
             (yy (cadr c-date))
             (y (+ (* 100 cycle) yy)))
        (diary-chinese-anniversary lunar-month lunar-day y mark))
    (diary-chinese-anniversary lunar-month lunar-day year mark)))

(setq calendar-holidays
      '(;;公历节日
        (holiday-fixed 1 1 "元旦")
        (holiday-fixed 3 8 "妇女节")
        (holiday-fixed 5 1 "劳动节")
        (holiday-fixed 6 1 "儿童节")
        (holiday-fixed 9 10 "教师节")
        (holiday-fixed 10 1 "国庆节")
        (holiday-fixed 10 2 "国庆节")
        (holiday-fixed 10 3 "国庆节")
        ;;农历节日
        (holiday-chinese-new-year)
        (holiday-lunar 12 30 "除夕" 0)
        (holiday-lunar 1 1 "春节" 0)
        (holiday-lunar 1 2 "春节" 0)
        (holiday-lunar 1 3 "春节" 0)
        (holiday-lunar 1 15 "元宵节" 0)
        (holiday-solar-term "清明" "清明节")
        (holiday-lunar 5 5 "端午节" 0)
        (holiday-lunar 7 7 "七夕节" 0)
        (holiday-lunar 8 15 "中秋节" 0)
        ;;其它节日
        (holiday-fixed 2 14 "情人节")
        (holiday-fixed 4 1 "愚人节")
        (holiday-fixed 12 25 "圣诞节")
        (holiday-float 5 0 2 "母亲节")
        (holiday-float 6 0 3 "父亲节")
        (holiday-float 11 4 4 "感恩节")
        ;;纪念日，已放入ann.org，此处注释备存
        ;; (holiday-fixed 9 16 "儿子生日")
        ;; (holiday-lunar 8 12 "儿子农历生日" 0)
        ;; (holiday-fixed 1 17 "老婆生日")
        ;; (holiday-lunar 12 17 "老婆农历生日" 0)
        ;; (holiday-lunar 1 2 "爸爸生日" 0)
        ;; (holiday-fixed 2 18 "爸爸公历生日")
        ;; (holiday-lunar 9 2 "妈妈生日" 0)
        ;; (holiday-fixed 10 9 "妈妈公历生日")
        ;; (holiday-fixed 7 12 "领证纪念日")
        ;; (holiday-fixed 10 4 "结婚纪念日")
        ;; (holiday-fixed 3 19 "我的生日")
        ;; (holiday-lunar 2 19 "我的农历生日" 0)
        ))
(setq mark-holidays-in-calendar t)
(setq calendar-mark-holidays-flag t)
(setq calendar-week-start-day 1)   ;;按中国习惯，周一为每周的第一天
