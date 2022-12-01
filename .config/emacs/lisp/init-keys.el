;; describe character under point
(keymap-global-set "C-h c" #'describe-char) ; describe-key-briefly

;; use much more powerful IBuffer mode to view buffers
(keymap-global-set "C-x C-b" #'ibuffer)

;; switch windows with ease
(keymap-global-set "M-o" #'other-window)
(keymap-global-set "M-O" #'mz/other-window-backwards)

;; use own command to un-/comment lines
(keymap-global-set "M-;" #'mz/comment-line)

;; key bindings to invoke Org features
(keymap-global-set "C-c a" #'org-agenda)
(keymap-global-set "C-c c" #'org-capture)
(keymap-global-set "C-c l" #'org-store-link)

;; insert current date
(keymap-global-set "C-c d" #'mz/insert-current-date)

;; toggle focus mode
(keymap-global-set "C-c f" #'focus-mode)

;; prettify JSON from clipboard
(keymap-global-set "C-c j" #'mz/display-json-from-clipboard)

;; lookup word at point
(keymap-global-set "C-c L" #'dictionary-lookup-definition)

;; start terminal
(keymap-global-set "C-c t" #'mz/ansi-term)

;; change key bindings in different modes
(with-eval-after-load 'doc-view
  (keymap-set doc-view-mode-map "<home>" #'doc-view-first-page)
  (keymap-set doc-view-mode-map "<end>" #'doc-view-last-page))

(with-eval-after-load 'elisp-mode
  ;; use globally bound command instead of local one
  (keymap-set lisp-interaction-mode-map "C-j" nil)) ; eval-print-last-sexp

(with-eval-after-load 'flymake
  (keymap-set flymake-mode-map "M-n" #'flymake-goto-next-error)
  (keymap-set flymake-mode-map "M-p" #'flymake-goto-prev-error))

(with-eval-after-load 'help-mode
  (keymap-set help-mode-map "n" #'next-line)
  (keymap-set help-mode-map "p" #'previous-line))

(with-eval-after-load 'ibuffer
  ;; use globally bound command instead of local one
  (keymap-set ibuffer-mode-map "M-o" nil)) ; ibuffer-visit-buffer-1-window

(with-eval-after-load 'info
  (keymap-set Info-mode-map "n" #'next-line)
  (keymap-set Info-mode-map "p" #'previous-line)
  (keymap-set Info-mode-map "N" #'Info-next)
  (keymap-set Info-mode-map "P" #'Info-prev))

(with-eval-after-load 'man
  (keymap-set Man-mode-map "n" #'next-line)
  (keymap-set Man-mode-map "p" #'previous-line)
  (keymap-set Man-mode-map "N" #'Man-next-section)
  (keymap-set Man-mode-map "P" #'Man-previous-section))

(provide 'init-keys)
