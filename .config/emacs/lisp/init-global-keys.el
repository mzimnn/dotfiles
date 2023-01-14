;; describe character under point
(global-set-key (kbd "C-h c") #'describe-char) ; describe-key-briefly

;; use much more powerful IBuffer mode to view buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; switch windows with ease
(global-set-key (kbd "M-o") #'other-window)
(global-set-key (kbd "M-O") #'mz/other-window-backwards)

;; keybindings to invoke Org features
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)

;; insert current date
(global-set-key (kbd "C-c d") #'mz/insert-current-date)

;; toggle focus mode
(global-set-key (kbd "C-c f") #'olivetti-mode)

;; start terminal
(global-set-key (kbd "C-c t") #'mz/ansi-term)

(provide 'init-global-keys)
