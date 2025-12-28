;; -*- lexical-binding: t; -*-

;; ignore case when completing
(setopt completion-ignore-case t)
(setopt read-file-name-completion-ignore-case t)
(setopt read-buffer-completion-ignore-case t)

;; show completion popup in GUI
(setopt corfu-auto t)
(setopt corfu-quit-no-match t)
(global-corfu-mode)

(provide 'init-completion)
