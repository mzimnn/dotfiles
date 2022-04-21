;; set up package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; download Evil
(unless (package-installed-p 'evil)
  (package-refresh-contents)
  (package-install 'evil))

;; download Ledger-mode
(unless (package-installed-p 'ledger-mode)
  (package-refresh-contents)
  (package-install 'ledger-mode))

;; download Magit
(unless (package-installed-p 'magit)
  (package-refresh-contents)
  (package-install 'magit))

;; disable CTRL-i to allow using tab in terminal
(unless (display-graphic-p)
  (setq evil-want-C-i-jump nil))

;; enable CTRL-u to scroll upwards
(setq evil-want-C-u-scroll t)

;; enable Evil
(require 'evil)
(evil-mode -1)

;; enable Org Indent mode
(setq org-startup-indented t)

;; hide emphasis markers
(setq org-hide-emphasis-markers t)

;; customize TODO keywords
(setq org-todo-keywords
    '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" )))

;; keybindings to invoke Org features
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)
;; use much more powerful IBuffer mode to view buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; move customization settings into own file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file)
    (load custom-file))

;; show relative line numbers
(add-hook 'conf-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(setq display-line-numbers-type 'visual)

;; show current column
(setq column-number-mode t)

;; hide welcome screen
(setq inhibit-startup-screen t)

;; specify when to wrap lines
(setq-default fill-column 80)
(add-hook 'git-commit-setup-hook (lambda () (setq fill-column 72)))

;; use Command key as Meta key
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

;; load theme
(load-theme 'deeper-blue)

;; save bookmarks after each change
(setq bookmark-save-flag 1)

;; enable tab-bar-mode but hide the tab bar
(setq tab-bar-new-tab-choice "*scratch*")
(setq tab-bar-show nil)
(tab-bar-mode 1)

;; hide visual elements
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
