(defun mz/configure-modes-for-editing ()
  "Configure modes for editing."
  (unless buffer-read-only
    (display-line-numbers-mode)
    (whitespace-mode))
  (setq truncate-lines t))

(defun mz/configure-conf-modes ()
  "Configure modes which derive from `conf-mode'."
  (mz/configure-modes-for-editing))

(defun mz/configure-prog-modes ()
  "Configure modes which derive from `prog-mode'."
  (mz/configure-modes-for-editing)
  (hl-todo-mode)
  (when (and (not buffer-read-only)
             (mz/ispell-program-installed-p))
    (flyspell-prog-mode)))

(defun mz/configure-text-modes ()
  "Configure modes which derive from `text-mode'."
  (mz/configure-modes-for-editing))

(add-hook 'conf-mode-hook #'mz/configure-conf-modes)
(add-hook 'prog-mode-hook #'mz/configure-prog-modes)
(add-hook 'text-mode-hook #'mz/configure-text-modes)

;; center buffer content for some modes
(dolist (hook '( eww-mode-hook Info-mode-hook man-common-hook ))
  (add-hook hook #'olivetti-mode))

;; make URLs and email addresses clickable
(add-hook 'shell-mode-hook #'goto-address-mode)

;; specify when to wrap lines
(setq-default fill-column 80)

;; use Command key as Meta key
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

;; only scroll current line horizontally
(setq auto-hscroll-mode 'current-line)
;; allow moving point to top-/bottom-most position when scrolling
(setq scroll-error-top-bottom t)

;; avoid polling when possible to save CPU cycles
(setq auto-revert-avoid-polling t)
;; don't display a message when a buffer has been reverted
(setq auto-revert-verbose nil)

;; save bookmarks after each change
(setq bookmark-save-flag 1)

;; setup all Ediff windows in the current frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; do stop at start/end of buffer when moving to errors
(setq flymake-wrap-around nil)

;; show number of matches in search prompt
(setq isearch-lazy-count t)

;; don't ask when saving personal dictionary
(setq ispell-silently-savep t)

;; don't show warnings during native compilation
(setq native-comp-async-report-warnings-errors 'silent)

;; type less backslashes
(setq reb-re-syntax 'string)

;; let a single space end a sentence
(setq sentence-end-double-space nil)

;; ensure `switch-to-buffer' respects display actions
(setq switch-to-buffer-obey-display-actions t)

;; flash frame to represent a bell
(setq visible-bell t)

;; replace selection with typed text
(delete-selection-mode)

;; allow repeating commands more easily
(repeat-mode)

;; specify width of TAB character
(setq-default tab-width 4)

;; guess indentation settings
(setq dtrt-indent-verbosity 0)
(dtrt-indent-global-mode)

;; enable tab-bar-mode
(setq tab-bar-close-button-show nil)
(setq tab-bar-format
      '(tab-bar-format-history tab-bar-format-tabs tab-bar-separator))
(setq tab-bar-new-tab-choice "*scratch*")
(setq tab-bar-show 1)
(tab-bar-mode 1)

;; automatically resize minibuffer to fit input text
(setq resize-mini-windows t)
;; only show default argument in unchanged minibuffer
(minibuffer-electric-default-mode)

;; enable commands which are disabled by default
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; inspired by https://www.emacswiki.org/emacs/HalfScrolling
(defun mz/scroll-half-window-height (&optional arg)
  "Set `next-screen-context-lines' to half window height.

It can be used to advice `scroll-down' and `scroll-up'."
  (setq next-screen-context-lines (max 1 (/ (1- (window-height)) 2))))

(advice-add 'scroll-down :before #'mz/scroll-half-window-height)
(advice-add 'scroll-up :before #'mz/scroll-half-window-height)

(provide 'init-misc)
