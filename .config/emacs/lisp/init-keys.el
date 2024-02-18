;; define leader map
(define-prefix-command 'mz/leader-map)
(keymap-global-set "C-c" mz/leader-map)

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
(keymap-set mz/leader-map "a" #'org-agenda)
(keymap-set mz/leader-map "c" #'org-capture)
(keymap-set mz/leader-map "l" #'org-store-link)

;; insert current date
(keymap-set mz/leader-map "d" #'mz/insert-current-date)

;; toggle focus mode
(keymap-set mz/leader-map "f" #'focus-mode)

;; prettify JSON from clipboard
(keymap-set mz/leader-map "j" #'mz/display-json-from-clipboard)

;; lookup word at point
(keymap-set mz/leader-map "L" #'dictionary-lookup-definition)

;; start terminal
(keymap-set mz/leader-map "t" #'mz/ansi-term)

;; change key bindings in different modes
(with-eval-after-load 'doc-view
  (keymap-set doc-view-mode-map "<home>" #'doc-view-first-page)
  (keymap-set doc-view-mode-map "<end>" #'doc-view-last-page))

(with-eval-after-load 'elisp-mode
  ;; use globally bound command instead of local one
  (keymap-set lisp-interaction-mode-map "C-j" nil)) ; eval-print-last-sexp

(with-eval-after-load 'evil
  ;; use globally bound command instead of local one
  (keymap-set evil-normal-state-map "M-." nil)) ; evil-repeat-pop-next

(with-eval-after-load 'eww
  ;; change key bindings in eww mode
  (keymap-set eww-mode-map "n" #'next-line)
  (keymap-set eww-mode-map "p" #'previous-line)
  (keymap-set eww-mode-map "N" #'eww-next-url)
  (keymap-set eww-mode-map "P" #'eww-previous-url)
  (keymap-set eww-mode-map "V" #'eww-view-source)
  (keymap-set eww-mode-map "v" nil)
  (keymap-set eww-mode-map "M-s" #'mz/eww-display-content-length)

  ;; change key bindings in eww bookmarks mode
  (keymap-set eww-bookmark-mode-map "n" #'next-line)
  (keymap-set eww-bookmark-mode-map "p" #'previous-line))

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

(with-eval-after-load 'profiler
  (keymap-set profiler-report-mode-map "RET" #'profiler-report-find-entry))

(with-eval-after-load 'term
  (keymap-set term-raw-map "M-o" nil)
  (keymap-set term-raw-map "M-x" nil))

(provide 'init-keys)
