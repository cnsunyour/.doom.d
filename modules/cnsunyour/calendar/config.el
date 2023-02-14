;;; ~/.doom.d/+calendar.el -*- lexical-binding: t; -*-

;; 日历及纪念日相关设置

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

(setq calendar-mark-holidays-flag t
      calendar-week-start-day 1)

(use-package! cal-china-x
  :custom
  (cal-china-x-important-holidays '((holiday-chinese-new-year)
                                    (holiday-lunar 12 23 "小年(北)" 1)
                                    (holiday-lunar 12 24 "小年(南)" 1)
                                    (holiday-lunar 12 30 "除夕" 1)
                                    (holiday-lunar 1 1 "春节" 0)
                                    (holiday-lunar 1 2 "春节" 0)
                                    (holiday-lunar 1 3 "春节" 0)
                                    (holiday-lunar 1 4 "春节" 0)
                                    (holiday-lunar 1 5 "春节(破五)" 0)
                                    (holiday-lunar 1 15 "元宵节" 0)
                                    (holiday-lunar 2 2 "龙抬头" 0)
                                    (holiday-solar-term "清明" "清明节")
                                    (holiday-fixed 5 1 "劳动节")
                                    (holiday-lunar 5 5 "端午节" 0)
                                    (holiday-lunar 7 7 "七夕节" 0)
                                    (holiday-lunar 7 15 "中元节" 0)
                                    (holiday-lunar 8 15 "中秋节" 0)
                                    (holiday-fixed 10 1 "国庆节")
                                    (holiday-fixed 10 2 "国庆节")
                                    (holiday-fixed 10 3 "国庆节")
                                    (holiday-lunar 9 9 "重阳节" 0)
                                    (holiday-lunar 10 1 "寒衣节" 0)
                                    (holiday-lunar 12 8 "腊八" 1)
                                    (holiday-solar-term "立春" "立春")
                                    (holiday-solar-term "立夏" "立夏")
                                    (holiday-solar-term "立秋" "立秋")
                                    (holiday-solar-term "立冬" "立冬")
                                    (holiday-solar-term "春分" "春分")
                                    (holiday-solar-term "夏至" "夏至")
                                    (holiday-solar-term "秋分" "秋分")
                                    (holiday-solar-term "冬至" "冬至")))
  (cal-china-x-general-holidays '((holiday-fixed 1 1 "元旦")
                                  (holiday-fixed 2 14 "情人节")
                                  (holiday-fixed 3 8 "妇女节")
                                  (holiday-fixed 4 1 "愚人节")
                                  (holiday-fixed 5 4 "青年节")
                                  (holiday-float 5 0 2 "母亲节")
                                  (holiday-fixed 6 1 "儿童节")
                                  (holiday-float 6 0 3 "父亲节")
                                  (holiday-fixed 9 10 "教师节")
                                  (holiday-float 11 4 4 "感恩节")
                                  (holiday-fixed 12 25 "圣诞节")))
  :config
  (setq calendar-holidays (append cal-china-x-important-holidays
                                  cal-china-x-general-holidays)))
