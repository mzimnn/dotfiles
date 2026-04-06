;; -*- lexical-binding: t; -*-

;; Specify when to wrap lines
(setopt fill-column 80)

;; By default use spaces for indentation
(setopt indent-tabs-mode nil)

;; Specify width of TAB character
(setopt tab-width 4)

;; Let a single space end a sentence
(setopt sentence-end-double-space nil)

;; Replace selection with typed text
(delete-selection-mode)

;; Guess indentation settings
(setopt dtrt-indent-verbosity 0)
(dtrt-indent-global-mode)

(provide 'init-editing)
