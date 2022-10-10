;; use much more powerful IBuffer mode to view buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; toggle focus mode
(global-set-key (kbd "C-c f") #'olivetti-mode)

;; hide mode line in focus mode
(add-hook 'olivetti-mode-on-hook #'turn-on-hide-mode-line-mode)
(add-hook 'olivetti-mode-off-hook #'turn-off-hide-mode-line-mode)

(defun mz/adjust-environment-for-editing ()
  "Enable useful minor modes and set useful variables for editing."
  (display-line-numbers-mode)
  (whitespace-mode)
  (setq truncate-lines t))

(add-hook 'conf-mode-hook 'mz/adjust-environment-for-editing)
(add-hook 'prog-mode-hook 'mz/adjust-environment-for-editing)
(add-hook 'text-mode-hook 'mz/adjust-environment-for-editing)

;; truncate lines if they do not fit on the screen
(add-hook 'dired-mode-hook (lambda () (setq truncate-lines 1)))

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

;; ignore case when completing
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)

;; let a single space end a sentence
(setq sentence-end-double-space nil)

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

;; highlight uncommitted changes
;; TODO: Enable diff-hl-margin-mode if Emacs runs in the terminal. It's not easy
;; though to check that. For example it is not possible to just call
;; display-graphic-p here, since Emacs can also be started as a daemon. In that
;; case this function would always return nil.
(setq diff-hl-show-staged-changes nil)
(global-diff-hl-mode)
(diff-hl-flydiff-mode)
(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

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

(defun mz/scroll-half-window-height (&optional arg)
  "Set `next-screen-context-lines' to half window height.

It can be used to advice `scroll-down' and `scroll-up'."
  (setq next-screen-context-lines (max 1 (/ (1- (window-height)) 2))))

(advice-add 'scroll-down :before #'mz/scroll-half-window-height)
(advice-add 'scroll-up :before #'mz/scroll-half-window-height)

(provide 'init-misc)
