;; define which modes use Evil by default
(setq evil-default-state 'emacs)
(setq evil-insert-state-modes '(git-commit-mode))
(setq evil-motion-state-modes nil)
(setq evil-normal-state-modes '(conf-mode prog-mode text-mode))

;; use Emacs key bindings in insert state
(setq evil-disable-insert-state-bindings t)

;; display a vertical bar in Emacs state
(setq evil-emacs-state-cursor 'bar)

;; certain commands should move point to start of line
(setq evil-start-of-line t)

;; search for symbols instead of words
(setq evil-symbol-word-search t)

;; allow usage of TAB key (e.g. to toggle headings in Org mode)
;; TODO: this should only apply in Org mode
(setq evil-want-C-i-jump nil)

;; use "Y" to yank to the end of line
(setq evil-want-Y-yank-to-eol t)

;; enable Evil
(evil-mode)

(provide 'init-evil)
