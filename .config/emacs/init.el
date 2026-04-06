;; -*- lexical-binding: t; -*-

;; Set up package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-unsigned-archives "melpa")
(setopt package-archive-priorities
        '(("gnu" . 20)
          ("nongnu" . 10)
          ("melpa" . 0)))

;; Install packages
(setopt package-check-signature t)
(setopt package-selected-packages
        '( corfu csv-mode diff-hl dtrt-indent evil hide-mode-line hl-todo
           ledger-mode magit markdown-mode olivetti org-roam ))

(unless (seq-every-p #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (package-install-selected-packages t))

;; Use local directory to modularize this init file
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

;; Load own modes
(require 'focus-mode)

;; Load utility functions
(require 'init-utils)

;; Load separate init files
(require 'init-calendar)
(require 'init-completion)
(require 'init-dired)
(require 'init-editing)
(require 'init-evil)
(require 'init-eww)
(require 'init-git)
(require 'init-misc)
(require 'init-org)
(require 'init-term)
(require 'init-visual)

;; Load key bindings
(require 'init-keys)

;; Load machine-specific configurations if available
(require 'init-local nil t)

;; Move customization settings into own file but do NOT load them
(setopt custom-file (expand-file-name "custom.el" user-emacs-directory))
