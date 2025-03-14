;; set up package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; install packages
(setopt package-selected-packages
        '( corfu csv-mode diff-hl dtrt-indent evil haskell-mode hide-mode-line
           hl-todo ledger-mode magit markdown-mode olivetti org-roam ))

(unless (seq-every-p #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (package-install-selected-packages t))

;; use local directory to modularize this init file
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

;; load own modes
(require 'focus-mode)

;; load utility functions
(require 'init-utils)

;; load separate init files
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

;; load key bindings
(require 'init-keys)

;; load machine-specific configurations if available
(require 'init-local nil t)

;; move customization settings into own file but do NOT load them
(setopt custom-file (expand-file-name "custom.el" user-emacs-directory))
