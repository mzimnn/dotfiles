;; set up package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; download Evil
(unless (package-installed-p 'evil)
  (package-refresh-contents)
  (package-install 'evil))

;; enable CTRL-u to scroll upwards
(setq evil-want-C-u-scroll t)

;; enable Evil
(require 'evil)
(evil-mode 1)

;; hide emphasis markers
(setq org-hide-emphasis-markers t)

;; customize TODO keywords
(setq org-todo-keywords
    '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" )))

;; move customization settings into own file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file)
    (load custom-file))

;; show relative line numbers
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'visual)

;; show current column
(setq column-number-mode t)

;; hide welcome screen
(setq inhibit-startup-screen t)

;; specify when to wrap lines
(setq-default fill-column 80)

;; use Command key as Meta key
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

;; load theme
(load-theme 'adwaita)

