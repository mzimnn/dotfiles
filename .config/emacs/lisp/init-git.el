;; highlight too long commit summary
(setq git-commit-summary-max-length 50)

;; specify when to wrap lines
(add-hook 'git-commit-mode-hook (lambda () (setq fill-column 72)))

;; don't show relevant diff when committing
(setq magit-commit-show-diff nil)
;; highlight differences within a line
(setq magit-diff-refine-hunk 'all)
;; highlight whitespace changes
(setq magit-diff-refine-ignore-whitespace nil)
;; don't ask when saving buffers automatically
(setq magit-save-repository-buffers 'dontask)

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

(provide 'init-git)