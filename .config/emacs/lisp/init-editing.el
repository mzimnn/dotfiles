;; specify when to wrap lines
(setopt fill-column 80)

;; by default use spaces for indentation
(setopt indent-tabs-mode nil)

;; specify width of TAB character
(setopt tab-width 4)

;; let a single space end a sentence
(setopt sentence-end-double-space nil)

;; replace selection with typed text
(delete-selection-mode)

;; guess indentation settings
(setopt dtrt-indent-verbosity 0)
(dtrt-indent-global-mode)

(provide 'init-editing)
