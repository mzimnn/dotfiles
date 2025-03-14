;; define which modes use Evil by default
(setopt evil-default-state 'emacs)
(setopt evil-insert-state-modes '(git-commit-mode))
(setopt evil-motion-state-modes nil)
(setopt evil-normal-state-modes '(conf-mode prog-mode text-mode))

;; use Emacs key bindings in insert state
(setopt evil-disable-insert-state-bindings t)

;; display a vertical bar in Emacs state
(setopt evil-emacs-state-cursor 'bar)

;; certain commands should move point to start of line
(setopt evil-start-of-line t)

;; search for symbols instead of words
(setopt evil-symbol-word-search t)

;; allow usage of TAB key (e.g. to toggle headings in Org mode)
;; TODO: this should only apply in Org mode
(setopt evil-want-C-i-jump nil)

;; use "Y" to yank to the end of line
(setopt evil-want-Y-yank-to-eol t)

;; enable Evil
(evil-mode)

(provide 'init-evil)
