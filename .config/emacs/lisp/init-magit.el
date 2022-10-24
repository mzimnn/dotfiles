;; don't ask when saving buffers automatically
(setq magit-save-repository-buffers 'dontask)

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

(provide 'init-magit)
