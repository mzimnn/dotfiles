;; describe character under point
(global-set-key (kbd "C-h c") #'describe-char) ; describe-key-briefly

;; use much more powerful IBuffer mode to view buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; switch windows with ease
(global-set-key (kbd "M-o") #'other-window)
(global-set-key (kbd "M-O") #'mz/other-window-backwards)

;; use own command to un-/comment lines
(global-set-key (kbd "M-;") #'mz/comment-line)

;; key bindings to invoke Org features
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)

;; insert current date
(global-set-key (kbd "C-c d") #'mz/insert-current-date)

;; toggle focus mode
(global-set-key (kbd "C-c f") #'focus-mode)

;; lookup word at point
(global-set-key (kbd "C-c L") #'dictionary-lookup-definition)

;; start terminal
(global-set-key (kbd "C-c t") #'mz/ansi-term)

;; change key bindings in different modes
(with-eval-after-load 'doc-view
  (define-key doc-view-mode-map (kbd "<home>") #'doc-view-first-page)
  (define-key doc-view-mode-map (kbd "<end>") #'doc-view-last-page))

(with-eval-after-load 'elisp-mode
  (define-key lisp-interaction-mode-map (kbd "C-j") nil))

(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))

(with-eval-after-load 'help-mode
  (define-key help-mode-map "n" 'next-line)
  (define-key help-mode-map "p" 'previous-line))

(with-eval-after-load 'ibuffer
  ;; use globally bound command instead of local one
  (define-key ibuffer-mode-map (kbd "M-o") nil)) ; ibuffer-visit-buffer-1-window

(with-eval-after-load 'info
  (define-key Info-mode-map "n" 'next-line)
  (define-key Info-mode-map "p" 'previous-line)
  (define-key Info-mode-map "N" 'Info-next)
  (define-key Info-mode-map "P" 'Info-prev))

(provide 'init-keys)
