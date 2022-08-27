;; set up package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(defun mz/package-install-if-missing (wanted-pkgs)
  "Install packages defined in WANTED-PKGS if they are missing."
  (let ((missing-pkgs ()))
    (dolist (wanted-pkg wanted-pkgs)
      (unless (package-installed-p wanted-pkg)
        (setq missing-pkgs (cons wanted-pkg missing-pkgs))))
    (when (consp missing-pkgs)
      (package-refresh-contents)
      (dolist (missing-pkg missing-pkgs)
        (package-install missing-pkg)))))

;; install packages
(setq package-selected-packages
      '(diff-hl evil ledger-mode magit solarized-theme))
(mz/package-install-if-missing package-selected-packages)

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
      '((sequence "TODO(t)" "HOLD(h)" "WAIT(w)" "|" "DONE(d)" )))

;; use relative paths for links
(setq org-link-file-path-type 'relative)

;; keybindings to invoke Org features
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)
;; use much more powerful IBuffer mode to view buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; move customization settings into own file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

;; load machine-specific configurations
(load (expand-file-name "local.el" user-emacs-directory) t)

(defun mz/adjust-environment-for-editing ()
  "Enable useful minor modes and set useful variables for editing."
  (display-line-numbers-mode)
  (whitespace-mode)
  (setq truncate-lines t))

(add-hook 'conf-mode-hook 'mz/adjust-environment-for-editing)
(add-hook 'prog-mode-hook 'mz/adjust-environment-for-editing)
(add-hook 'text-mode-hook 'mz/adjust-environment-for-editing)

;; truncate lines if they do not fit on the screen
(add-hook 'dired-mode-hook (lambda () (setq truncate-lines 1)))

;; show relative line numbers
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
(setq solarized-height-minus-1 1.0)
(setq solarized-height-plus-1 1.0)
(setq solarized-height-plus-2 1.0)
(setq solarized-height-plus-3 1.0)
(setq solarized-height-plus-4 1.0)
(setq solarized-use-variable-pitch nil)
(setq x-underline-at-descent-line t)
(load-theme 'solarized-selenized-black t)
(require 'solarized-palettes)
;; TODO: use corresponding color palette of loaded theme
(setq mz/color-palette solarized-selenized-black-color-palette-alist)

(defun mz/get-theme-color (color)
  "Return value of COLOR defined in `mz/color-palette'."
  (alist-get color mz/color-palette))

;; improve visual appearance of tab bar
(custom-set-faces
 `(tab-bar
   ((t (:background ,(mz/get-theme-color 'base02)))))
 `(tab-bar-tab
   ((t (:box (:line-width (6 . 2) :color ,(face-attribute
                                           'tab-bar-tab
                                           :background))))))
 `(tab-bar-tab-inactive
   ((t (:box (:line-width (6 . 2) :color ,(face-attribute
                                           'tab-bar-tab-inactive
                                           :background)))))))

;; show spaces and tabs
(setq whitespace-style
      '(face tabs spaces trailing space-mark tab-mark))
(custom-set-faces
 `(whitespace-tab
   ((t (:foreground ,(mz/get-theme-color 'base01)
        :inverse-video unspecified))))
 `(whitespace-trailing
   ((t (:foreground unspecified
        :background ,(mz/get-theme-color 'red)
        :inverse-video unspecified)))))

;; ensure region only highlights text
(custom-set-faces '(region ((t (:extend nil)))))

;; save bookmarks after each change
(setq bookmark-save-flag 1)

;; set weekday on which a week begins
(setq calendar-week-start-day 1)

;; ignore case when completing
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)

;; let a single space end a sentence
(setq sentence-end-double-space nil)

;; wrap lines after column 80
(setq shr-max-width 80)
;; do not render colors
(setq shr-use-colors nil)
;; always use monospaced fonts
(setq shr-use-fonts nil)

;; allow repeating commands more easily
(repeat-mode)

;; enable tab-bar-mode
(setq tab-bar-close-button-show nil)
(setq tab-bar-format
      '(tab-bar-format-history tab-bar-format-tabs tab-bar-separator))
(setq tab-bar-new-tab-choice "*scratch*")
(setq tab-bar-show 1)
(tab-bar-mode 1)

;; highlight uncommitted changes
;; TODO: Enable diff-hl-margin-mode if Emacs runs in the terminal. It's not easy
;; though to check that. For example it is not possible to just call
;; display-graphic-p here, since Emacs can also be started as a daemon. In that
;; case this function would always return nil.
(setq diff-hl-show-staged-changes nil)
(global-diff-hl-mode)
(diff-hl-flydiff-mode)
(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;; only show default argument in unchanged minibuffer
(minibuffer-electric-default-mode)

;; hide visual elements
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; enable commands which are disabled by default
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; change keybindings in eww mode
(add-hook 'eww-mode-hook
          (lambda ()
            (local-set-key "n" 'next-line)
            (local-set-key "p" 'previous-line)
            (local-set-key "N" 'eww-next-url)
            (local-set-key "P" 'eww-previous-url)
            (local-set-key "V" 'eww-view-source)
            (local-unset-key "v")))

;; change keybindings in eww bookmarks mode
(add-hook 'eww-bookmark-mode-hook
          (lambda ()
            (local-set-key "n" 'next-line)
            (local-set-key "p" 'previous-line)))

;; change keybindings in help mode
(add-hook 'help-mode-hook
          (lambda ()
            (local-set-key "n" 'next-line)
            (local-set-key "p" 'previous-line)))

;; change keybindings in Info mode
(add-hook 'Info-mode-hook
          (lambda ()
            (local-set-key "n" 'next-line)
            (local-set-key "p" 'previous-line)
            (local-set-key "N" 'Info-next)
            (local-set-key "P" 'Info-prev)))

(defun mz/scroll-half-window-height (&optional arg)
  "Set `next-screen-context-lines' to half window height.

It can be used to advice `scroll-down' and `scroll-up'."
  (setq next-screen-context-lines (max 1 (/ (1- (window-height)) 2))))

(advice-add 'scroll-down :before #'mz/scroll-half-window-height)
(advice-add 'scroll-up :before #'mz/scroll-half-window-height)

;; auto-generate an instant commit message
(defun mz/retrieve-git-status-lines ()
  "Parse the output of `git status --porcelain' into a list."
  (mapcar (lambda (status-line)
            (list (substring status-line 0 2)
                  (substring status-line 3)))
          (magit-git-lines "status" "--porcelain")))

(defun mz/keep-staged-status-lines (status-lines)
  "Only keep STATUS-LINES which are staged."
  (seq-filter (lambda (status-line)
                ;; See git-status(1) for more information about the status codes
                (seq-contains-p '("M" "T" "A" "D" "R" "C")
                                (substring (car status-line) 0 1)))
              status-lines))

(defun mz/get-paths-from-status-lines (status-lines)
  "Retrieve the paths of the STATUS-LINES."
  (mapcan (lambda (status-line)
            (cdr status-line))
          status-lines))

(defun mz/sort-alphabetically (strings)
  "Sort STRINGS in alphabetical order."
  (sort strings (lambda (str1 str2)
                  (string-lessp (downcase str1) (downcase str2)))))

(defun mz/convert-to-instant-commit-msg (filenames)
  "Convert FILENAMES into an instant commit message."
  (if (length= filenames 0)
      nil
    (concat "Changes in "
            (cond ((length= filenames 1)
                   (car filenames))
                  (t
                   (concat
                    (mapconcat #'identity (butlast filenames) ", ")
                    " and " (car (last filenames))))))))

(defun mz/git-instant-commit ()
  "Commit staged changes with an instant commit message."
  (interactive)
  ;; magit-commit-assert checks if there are any staged changes, if not it
  ;; prompts the user if he/she wants to stage all unstaged changes. The
  ;; function magit-commit-create also invokes this assertion but this has to be
  ;; done before the commit message is generated. Only then all staged files are
  ;; included in the commit message.
  (when (magit-commit-assert ())
    (magit-commit-create
     `("-m"
       ,(mz/convert-to-instant-commit-msg
         (mz/sort-alphabetically
          (mapcar #'file-name-nondirectory
                  (mz/get-paths-from-status-lines
                   (mz/keep-staged-status-lines
                    (mz/retrieve-git-status-lines))))))))))

(add-hook 'magit-mode-hook
          (lambda ()
            (transient-append-suffix 'magit-commit "c"
              '("C" "Instant commit" mz/git-instant-commit))))
