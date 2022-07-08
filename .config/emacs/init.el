;; set up package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(defun mz/package-install-if-missing (wanted-pkgs)
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
      '(evil ledger-mode magit solarized-theme))
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
(setq solarized-height-minus-1 1.0)
(setq solarized-height-plus-1 1.0)
(setq solarized-height-plus-2 1.0)
(setq solarized-height-plus-3 1.0)
(setq solarized-height-plus-4 1.0)
(setq solarized-use-variable-pitch nil)
(setq x-underline-at-descent-line t)
(load-theme 'solarized-selenized-black t)

;; improve visual appearance of tab bar
(require 'solarized-palettes)
(custom-set-faces
 `(tab-bar
   ((t (:background ,(alist-get
                      'base02
                      solarized-selenized-black-color-palette-alist)))))
 `(tab-bar-tab
   ((t (:box (:line-width (6 . 2) :color ,(face-attribute
                                           'tab-bar-tab
                                           :background))))))
 `(tab-bar-tab-inactive
   ((t (:box (:line-width (6 . 2) :color ,(face-attribute
                                           'tab-bar-tab-inactive
                                           :background)))))))

;; save bookmarks after each change
(setq bookmark-save-flag 1)

;; ignore case when completing
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)

;; let a single space end a sentence
(setq sentence-end-double-space nil)

;; enable tab-bar-mode
(setq tab-bar-close-button-show nil)
(setq tab-bar-format
      '(tab-bar-format-history tab-bar-format-tabs tab-bar-separator))
(setq tab-bar-new-tab-choice "*scratch*")
(setq tab-bar-show 1)
(tab-bar-mode 1)

;; hide visual elements
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

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

;; auto-generate an instant commit message
(defun mz/retrieve-git-status-lines ()
  "Parses the output of `git status --porcelain' into a list."
  (mapcar (lambda (status-line)
            (list (substring status-line 0 2)
                  (substring status-line 3)))
          (magit-git-lines "status" "--porcelain")))

(defun mz/keep-staged-status-lines (status-lines)
  "Only keeps STATUS-LINES which are staged."
  (seq-filter (lambda (status-line)
                ;; See git-status(1) for more information about the status codes
                (seq-contains-p '("M" "T" "A" "D" "R" "C")
                                (substring (car status-line) 0 1)))
              status-lines))

(defun mz/get-paths-from-status-lines (status-lines)
  "Retrieves the paths of the STATUS-LINES."
  (mapcan (lambda (status-line)
            (cdr status-line))
          status-lines))

(defun mz/sort-alphabetically (strings)
  "Sorts STRINGS in alphabetical order."
  (sort strings (lambda (str1 str2)
                  (string-lessp (downcase str1) (downcase str2)))))

(defun mz/convert-to-instant-commit-msg (filenames)
  "Converts FILENAMES into an instant commit message."
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
  "Commits staged changes with an instant commit message."
  (interactive)
  ;; magit-commit-assert checks if there are any staged changes, if not it
  ;; prompts the user if he/she wants to stage all unstaged changes. The
  ;; function magit-commit-create also invokes the this assertion but this has
  ;; to be done before the commit message is generated. Only then all staged
  ;; files are included in the commit message.
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
