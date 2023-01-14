;; set up package.el to work with MELPA
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; install packages
(setq package-selected-packages
      '(diff-hl dtrt-indent ledger-mode magit hide-mode-line olivetti
                solarized-theme))

(unless (seq-every-p #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (package-install-selected-packages t))

;; use local directory to modularize this init file
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

;; load separate init files
(require 'init-dired)
(require 'init-eww)
(require 'init-magit)
(require 'init-misc)
(require 'init-org)
(require 'init-term)
(require 'init-visual)

;; load global key bindings
(require 'init-global-keys)

;; load machine-specific configurations
(require 'init-local nil t)

;; move customization settings into own file but do NOT load them
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
