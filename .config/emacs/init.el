;; set up package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; install packages
(setq package-selected-packages
      '(diff-hl ledger-mode magit solarized-theme))

(unless (seq-every-p #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (package-install-selected-packages))

;; use local directory to modularize this init file
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

;; load separate init files
(require 'init-eww)
(require 'init-magit)
(require 'init-misc)
(require 'init-org)
(require 'init-theme)

;; move customization settings into own file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

;; load machine-specific configurations
(load (expand-file-name "local.el" user-emacs-directory) t)
