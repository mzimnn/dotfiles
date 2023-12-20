;; specify when to wrap lines
(setq-default fill-column 80)

;; let a single space end a sentence
(setq sentence-end-double-space nil)

;; replace selection with typed text
(delete-selection-mode)

;; specify width of TAB character
(setq-default tab-width 4)

;; guess indentation settings
(setq dtrt-indent-verbosity 0)
(dtrt-indent-global-mode)

;; by default use spaces for indentation
(setq-default indent-tabs-mode nil)

(provide 'init-editing)
