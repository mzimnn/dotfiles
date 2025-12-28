;; -*- lexical-binding: t; -*-

(defun mz/configure-modes-for-editing ()
  "Configure modes for editing."
  (unless buffer-read-only
    (display-line-numbers-mode)
    (whitespace-mode))
  (setq-local truncate-lines t))

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
  (mz/configure-modes-for-editing)
  (auto-fill-mode))

(add-hook 'conf-mode-hook #'mz/configure-conf-modes)
(add-hook 'prog-mode-hook #'mz/configure-prog-modes)
(add-hook 'text-mode-hook #'mz/configure-text-modes)

;; center buffer content for some modes
(dolist (hook '( eww-mode-hook Info-mode-hook man-common-hook ))
  (add-hook hook #'olivetti-mode))

;; truncate lines in some modes
(dolist (hook '( grep-mode-hook occur-mode-hook ))
  (add-hook hook (lambda () (setq-local truncate-lines t))))

;; make URLs and email addresses clickable
(add-hook 'shell-mode-hook #'goto-address-mode)
;; don't display output of asynchronous commands
(add-to-list 'display-buffer-alist
             `(,shell-command-buffer-name-async (display-buffer-no-window)))

;; use Command key as Meta key
(setopt mac-option-modifier 'super)
(setopt mac-command-modifier 'meta)

;; only scroll current line horizontally
(setopt auto-hscroll-mode 'current-line)
;; allow moving point to top-/bottom-most position when scrolling
(setopt scroll-error-top-bottom t)

;; avoid polling when possible to save CPU cycles
(setopt auto-revert-avoid-polling t)
;; don't display a message when a buffer has been reverted
(setopt auto-revert-verbose nil)

;; improve performance of redisplay by disabling RTL text rendering
(setopt bidi-paragraph-direction 'left-to-right)

;; save bookmarks after each change
(setopt bookmark-save-flag 1)

;; setup all Ediff windows in the current frame
(setopt ediff-window-setup-function #'ediff-setup-windows-plain)

;; do not use multiple lines in the minibuffer
(setopt eldoc-echo-area-use-multiline-p nil)

;; do stop at start/end of buffer when moving to errors
(setopt flymake-wrap-around nil)

;; allow pixelwise resize of frames
(setopt frame-resize-pixelwise t)

;; show number of matches in search prompt
(setopt isearch-lazy-count t)

;; don't ask when saving personal dictionary
(setopt ispell-silently-savep t)

;; don't show warnings during native compilation
(setopt native-comp-async-report-warnings-errors 'silent)

;; type less backslashes
(setopt reb-re-syntax 'string)

;; ensure `switch-to-buffer' respects display actions
(setopt switch-to-buffer-obey-display-actions t)

;; flash frame to represent a bell
(setopt visible-bell t)

;; allow repeating commands more easily
(repeat-mode)
;; do not repeat these commands to avoid conflicts with key "o" in Evil mode
(dolist (command '(other-window tab-next tab-previous))
  (put command 'repeat-map nil))

;; enable tab-bar-mode
(setopt tab-bar-close-button-show nil)
(setopt tab-bar-format
        '(tab-bar-format-history tab-bar-format-tabs tab-bar-separator))
(setopt tab-bar-new-tab-choice #'get-scratch-buffer-create)
(setopt tab-bar-show 1)
(tab-bar-mode)

;; shorten display of default prompt in minibuffer
(setopt minibuffer-default-prompt-format " [%s]")
;; automatically resize minibuffer to fit input text
(setopt resize-mini-windows t)
;; only show default argument in unchanged minibuffer
(minibuffer-electric-default-mode)

;; enable commands which are disabled by default
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; inspired by: https://www.emacswiki.org/emacs/HalfScrolling
(defun mz/scroll-half-window-height (&optional arg)
  "Set `next-screen-context-lines' to half window height.

It can be used to advice `scroll-down' and `scroll-up'."
  (setq next-screen-context-lines (max 1 (/ (1- (window-height)) 2))))

(advice-add 'scroll-down :before #'mz/scroll-half-window-height)
(advice-add 'scroll-up :before #'mz/scroll-half-window-height)

(provide 'init-misc)
