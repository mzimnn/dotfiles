;; ensure point blinks forever
(setopt blink-cursor-blinks -1)

;; show current column
(setopt column-number-mode t)

;; show relative line numbers
(setopt display-line-numbers-type 'visual)

;; hide welcome screen
(setopt inhibit-startup-screen t)

;; load theme
(setopt modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))
(load-theme (car modus-themes-to-toggle))

;; show tabs and trailing whitespace
(setopt whitespace-style
        '(face space-before-tab tabs trailing tab-mark))

(defun mz/set-custom-faces ()
  "Set custom faces."
  (modus-themes-with-colors
    (custom-set-faces
     ;; highlight background of symbol at point
     `(eglot-highlight-symbol-face ((,c (:background ,bg-inactive))))

     ;; adjust coloring of holidays
     `(holiday ((,c (:foreground ,yellow-warmer))))

     ;; ensure region only highlights text
     '(region ((t (:extend nil))))

     ;; improve visual appearance of tab bar
     `(tab-bar-tab
       ((,c (:box (:line-width (6 . 2) :color ,bg-tab-current)))))
     `(tab-bar-tab-inactive
       ((,c (:box (:line-width (6 . 2) :color ,bg-tab-other))))))

    ;; colorize priorities in Org mode
    (custom-set-variables
     `(org-priority-faces '((?A . ,red)
                            (?B . ,blue)
                            (?C . ,green))))))

;; load custom faces
(mz/set-custom-faces)

;; reload custom faces after a modus theme was loaded
(add-hook 'modus-themes-after-load-theme-hook #'mz/set-custom-faces)

(provide 'init-visual)
