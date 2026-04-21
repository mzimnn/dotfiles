;; -*- lexical-binding: t; -*-

;; Define global key bindings
(keymap-global-set "C-h c" #'describe-char) ; describe-key-briefly
(keymap-global-set "M-j" #'scroll-up-command)
(keymap-global-set "M-k" #'scroll-down-command)
(keymap-global-set "M-o" #'other-window)
(keymap-global-set "M-O" #'mz/other-window-backwards)
(keymap-global-set "<f12>" #'org-agenda-list)
(keymap-global-set "<remap> <comment-dwim>" #'mz/comment-line)
(keymap-global-set "<remap> <list-buffers>" #'ibuffer)

;; Define leader map
(define-prefix-command 'mz/leader-map)
(keymap-global-set "C-c" mz/leader-map)
(keymap-set mz/leader-map "a" #'org-agenda)
(keymap-set mz/leader-map "c" #'org-capture)
(keymap-set mz/leader-map "d" #'mz/insert-current-date)
(keymap-set mz/leader-map "F" #'focus-mode)
(keymap-set mz/leader-map "j" #'mz/display-json-from-clipboard)
(keymap-set mz/leader-map "l" #'org-store-link)
(keymap-set mz/leader-map "L" #'dictionary-lookup-definition)
(keymap-set mz/leader-map "n" #'mz/temp-buffer)
(keymap-set mz/leader-map "t" #'mz/ansi-term)

;; Define Org-roam map
(define-prefix-command 'mz/org-roam-map)
(keymap-set mz/leader-map "r" mz/org-roam-map)
(keymap-set mz/org-roam-map "a a" #'org-roam-alias-add)
(keymap-set mz/org-roam-map "a r" #'org-roam-alias-remove)
(keymap-set mz/org-roam-map "b" #'org-roam-buffer-toggle)
(keymap-set mz/org-roam-map "e" #'org-roam-extract-subtree)
(keymap-set mz/org-roam-map "f" #'org-roam-node-find)
(keymap-set mz/org-roam-map "g" #'org-roam-graph)
(keymap-set mz/org-roam-map "i" #'org-roam-node-insert)
(keymap-set mz/org-roam-map "r" #'org-roam-node-random)

;; Change key bindings in different modes
(with-eval-after-load 'diff-mode
  ;; Use globally bound command instead of local one
  (keymap-set diff-mode-map "M-k" nil) ; diff-hunk-kill
  (keymap-set diff-mode-map "M-o" nil)) ; diff-goto-source

(with-eval-after-load 'doc-view
  (keymap-set doc-view-mode-map "<home>" #'doc-view-first-page)
  (keymap-set doc-view-mode-map "<end>" #'doc-view-last-page))

(with-eval-after-load 'elisp-mode
  ;; Use globally bound command instead of local one
  (keymap-set lisp-interaction-mode-map "C-j" nil)) ; eval-print-last-sexp

(with-eval-after-load 'evil
  ;; Use globally bound command instead of local one
  (keymap-set evil-normal-state-map "M-." nil)) ; evil-repeat-pop-next

(with-eval-after-load 'eww
  ;; Change key bindings in eww mode
  (keymap-set eww-mode-map "n" #'next-line)
  (keymap-set eww-mode-map "p" #'previous-line)
  (keymap-set eww-mode-map "N" #'eww-next-url)
  (keymap-set eww-mode-map "P" #'eww-previous-url)
  (keymap-set eww-mode-map "V" #'eww-view-source)
  (keymap-set eww-mode-map "v" nil)
  (keymap-set eww-mode-map "M-s" #'mz/eww-display-content-length)
  (keymap-set eww-mode-map "{" #'backward-paragraph)
  (keymap-set eww-mode-map "}" #'forward-paragraph)

  ;; Change key bindings in eww bookmarks mode
  (keymap-set eww-bookmark-mode-map "n" #'next-line)
  (keymap-set eww-bookmark-mode-map "p" #'previous-line))

(with-eval-after-load 'flymake
  (keymap-set flymake-mode-map "M-n" #'flymake-goto-next-error)
  (keymap-set flymake-mode-map "M-p" #'flymake-goto-prev-error))

(with-eval-after-load 'help-mode
  (keymap-set help-mode-map "n" #'next-line)
  (keymap-set help-mode-map "p" #'previous-line))

(with-eval-after-load 'ibuffer
  ;; Use globally bound command instead of local one
  (keymap-set ibuffer-mode-map "M-j" nil)  ; ibuffer-jump-to-filter-group
  (keymap-set ibuffer-mode-map "M-o" nil)) ; ibuffer-visit-buffer-1-window

(with-eval-after-load 'info
  (keymap-set Info-mode-map "n" #'next-line)
  (keymap-set Info-mode-map "p" #'previous-line)
  (keymap-set Info-mode-map "N" #'Info-next)
  (keymap-set Info-mode-map "P" #'Info-prev)
  (keymap-set Info-mode-map "{" #'backward-paragraph)
  (keymap-set Info-mode-map "}" #'forward-paragraph))

(with-eval-after-load 'man
  (keymap-set Man-mode-map "n" #'next-line)
  (keymap-set Man-mode-map "p" #'previous-line)
  (keymap-set Man-mode-map "N" #'Man-next-section)
  (keymap-set Man-mode-map "P" #'Man-previous-section))

(with-eval-after-load 'profiler
  (keymap-set profiler-report-mode-map "RET" #'profiler-report-find-entry))

(with-eval-after-load 'sgml-mode
  (keymap-set html-mode-map "C-c M-o" facemenu-keymap)
  ;; Use globally bound command instead of local one
  (keymap-set html-mode-map "M-o" nil)) ; facemenu-keymap

(with-eval-after-load 'tab-bar
  (keymap-set tab-prefix-map "!" #'tab-window-detach))

(with-eval-after-load 'term
  (keymap-set term-raw-map "M-o" nil)
  (keymap-set term-raw-map "M-x" nil))

(provide 'init-keys)
