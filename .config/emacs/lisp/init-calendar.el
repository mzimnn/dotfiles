;; add aliases
(defalias 'cal #'calendar)

;; display ISO week numbers
(setopt calendar-intermonth-text
        '(propertize
          (format "%2d"
                  (car
                   (calendar-iso-from-absolute
                    (calendar-absolute-from-gregorian (list month day year)))))
          'font-lock-face 'font-lock-function-name-face))
;; highlight holidays in calendar
(setopt calendar-mark-holidays-flag t)
;; set weekday on which a week begins
(setopt calendar-week-start-day 1)

;; highlight today
(add-hook 'calendar-today-visible-hook #'calendar-mark-today)

(provide 'init-calendar)
