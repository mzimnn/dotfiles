;; use much more powerful IBuffer mode to view buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; toggle focus mode
(global-set-key (kbd "C-c f") #'olivetti-mode)
;; toggle whitespace mode
(global-set-key (kbd "C-c w") #'whitespace-mode)

(defun mz/other-window-backwards ()
  "Like `other-window' but in reverse."
  (interactive)
  (other-window -1))

;; switch windows with ease
(global-set-key (kbd "M-o") #'other-window)
(global-set-key (kbd "M-O") #'mz/other-window-backwards)

;; use globally bound command instead of local one
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (local-unset-key (kbd "M-o")))) ; ibuffer-visit-buffer-1-window

;; hide mode line in focus mode
(add-hook 'olivetti-mode-on-hook #'turn-on-hide-mode-line-mode)
(add-hook 'olivetti-mode-off-hook #'turn-off-hide-mode-line-mode)

(defun mz/insert-current-date (&optional arg)
  "Insert current date into buffer.

If called with a prefix ARG, use European format of date."
  (interactive "p")
  (insert (format-time-string (if (= arg 4) "%d.%m.%Y" "%Y-%m-%d"))))

;; insert current date
(global-set-key (kbd "C-c d") #'mz/insert-current-date)

(defun mz/adjust-environment-for-editing ()
  "Enable useful minor modes and set useful variables for editing."
  (display-line-numbers-mode)
  (whitespace-mode)
  (setq truncate-lines t))

(add-hook 'conf-mode-hook 'mz/adjust-environment-for-editing)
(add-hook 'prog-mode-hook 'mz/adjust-environment-for-editing)
(add-hook 'text-mode-hook 'mz/adjust-environment-for-editing)

;; truncate lines if they do not fit on the screen
(add-hook 'dired-mode-hook (lambda () (setq truncate-lines t)))

;; show relative line numbers
(setq display-line-numbers-type 'visual)

;; show current column
(setq column-number-mode t)

;; hide welcome screen
(setq inhibit-startup-screen t)

;; specify when to wrap lines
(setq-default fill-column 80)
(add-hook 'git-commit-setup-hook (lambda () (setq fill-column 72)))

;; use Command key as Meta key
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

;; save bookmarks after each change
(setq bookmark-save-flag 1)

;; set weekday on which a week begins
(setq calendar-week-start-day 1)

;; show number of matches in search prompt
(setq isearch-lazy-count t)

;; ignore case when completing
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)

;; type less backslashes
(setq reb-re-syntax 'string)

;; let a single space end a sentence
(setq sentence-end-double-space nil)

;; follow version-controlled symbolic links without asking
(setq vc-follow-symlinks t)

;; flash frame to represent a bell
(setq visible-bell t)

;; replace selection with typed text
(delete-selection-mode)

;; allow repeating commands more easily
(repeat-mode)

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

;; hide visual elements
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; enable commands which are disabled by default
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; change keybindings in help mode
(add-hook 'help-mode-hook
          (lambda ()
            (local-set-key "n" 'next-line)
            (local-set-key "p" 'previous-line)))

;; change keybindings in Info mode
(add-hook 'Info-mode-hook
          (lambda ()
            (local-set-key "n" 'next-line)
            (local-set-key "p" 'previous-line)
            (local-set-key "N" 'Info-next)
            (local-set-key "P" 'Info-prev)))

;; inspired by https://www.emacswiki.org/emacs/HalfScrolling
(defun mz/scroll-half-window-height (&optional arg)
  "Set `next-screen-context-lines' to half window height.

It can be used to advice `scroll-down' and `scroll-up'."
  (setq next-screen-context-lines (max 1 (/ (1- (window-height)) 2))))

(advice-add 'scroll-down :before #'mz/scroll-half-window-height)
(advice-add 'scroll-up :before #'mz/scroll-half-window-height)

(provide 'init-misc)
