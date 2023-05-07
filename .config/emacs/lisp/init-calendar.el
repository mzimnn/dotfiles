;; add aliases
(defalias 'cal #'calendar)

;; display ISO week numbers
(setq calendar-intermonth-text
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'font-lock-function-name-face))
;; highlight holidays in calendar
(setq calendar-mark-holidays-flag t)
;; set weekday on which a week begins
(setq calendar-week-start-day 1)

(provide 'init-calendar)
