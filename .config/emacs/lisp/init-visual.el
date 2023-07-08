;; ensure point blinks forever
(setq blink-cursor-blinks -1)

;; show current column
(setq column-number-mode t)

;; show relative line numbers
(setq display-line-numbers-type 'visual)

;; hide welcome screen
(setq inhibit-startup-screen t)

;; load theme
(require 'modus-themes)

(setq modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))
(load-theme (car modus-themes-to-toggle) t)

;; show tabs and trailing whitespace
(setq whitespace-style
      '(face space-before-tab tabs trailing tab-mark))

(defun mz/set-custom-faces ()
  "Set custom faces."
  (modus-themes-with-colors
    (custom-set-faces
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
