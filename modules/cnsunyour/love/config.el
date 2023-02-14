;;; cnsunyour/love/config.el -*- lexical-binding: t; -*-

(defun cnsunyour/lover-live-time (name year month day hour minute second)
  "Display the live time of my true love."
  (let* ((birth-time (encode-time second minute hour day month year))
         (live-time  (time-subtract (current-time) birth-time))
         (lt-secs    (float-time live-time)))
    (message
     (format "【%s】-> %d天 | %.2f月 | %.2f周 | %s"
             name
             (floor (/ lt-secs 86400))
             (/ lt-secs 2628000) ;; 1 y = 12 m, 1 m ~= 30.4166667 d
             (/ lt-secs 604800)
             (format-seconds "%y年%d天%h时%m分%z" lt-secs)))))

