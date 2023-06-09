;; ignore case when completing
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)

;; show completion popup in GUI
(setq corfu-auto t)
(setq corfu-quit-no-match t)
(global-corfu-mode)

(provide 'init-completion)
