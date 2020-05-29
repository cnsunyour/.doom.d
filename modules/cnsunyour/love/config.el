;;; cnsunyour/love/config.el -*- lexical-binding: t; -*-

(defun cnsunyour/lover-live-time (name year month day hour minute second)
  "Display the live time of my true love."
  (interactive)
  (let* ((birth-time (encode-time second minute hour day month year))
         (live-time  (time-subtract (current-time) birth-time))
         (lt-secs    (float-time live-time)))
    (message
     (format "%s: %d days; %.2f months; %.2f weeks; -- %s"
             name
             (floor (/ lt-secs 86400))
             (/ lt-secs 2628000) ;; 1 y = 12 m, 1 m ~= 30.4166667 d
             (/ lt-secs 604800)
             (format-seconds "%Y, %D, %H, %M%z" lt-secs)))))

(map! :leader
      (:prefix-map ("k" . "Lover's live time")
        :desc "Twinkle's live time" "k" (cmd! (cnsunyour/lover-live-time "Twinkle" 2013 9 16 13 43 0))
        :desc "Marry passed time" "m" (cmd! (cnsunyour/lover-live-time "Married" 2002 10 4 9 0 0))))
