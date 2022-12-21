;; load theme
(setq solarized-height-minus-1 1.0)
(setq solarized-height-plus-1 1.0)
(setq solarized-height-plus-2 1.0)
(setq solarized-height-plus-3 1.0)
(setq solarized-height-plus-4 1.0)
(setq solarized-use-variable-pitch nil)
(setq x-underline-at-descent-line t)
(load-theme 'solarized-selenized-black t)
(require 'solarized-palettes)
;; TODO: use corresponding color palette of loaded theme
(setq mz/color-palette solarized-selenized-black-color-palette-alist)

(defun mz/get-theme-color (color)
  "Return value of COLOR defined in `mz/color-palette'."
  (alist-get color mz/color-palette))

;; improve visual appearance of tab bar
(custom-set-faces
 `(tab-bar
   ((t (:background ,(mz/get-theme-color 'base02)))))
 `(tab-bar-tab
   ((t (:box (:line-width (6 . 2) :color ,(face-attribute
                                           'tab-bar-tab
                                           :background))))))
 `(tab-bar-tab-inactive
   ((t (:box (:line-width (6 . 2) :color ,(face-attribute
                                           'tab-bar-tab-inactive
                                           :background)))))))

;; show spaces and tabs
(setq whitespace-style
      '(face space-before-tab tabs trailing tab-mark))
(custom-set-faces
 `(whitespace-tab
   ((t (:foreground ,(mz/get-theme-color 'base01)
        :inverse-video unspecified))))
 `(whitespace-trailing
   ((t (:foreground unspecified
        :background ,(mz/get-theme-color 'red)
        :inverse-video unspecified)))))

;; ensure region only highlights text
(custom-set-faces '(region ((t (:extend nil)))))

;; colorize priorities in Org mode
(custom-set-variables
 '(org-priority-faces `((?A . ,(mz/get-theme-color 'red))
                        (?B . ,(mz/get-theme-color 'blue))
                        (?C . ,(mz/get-theme-color 'green)))))

(provide 'init-theme)
