;; dereference symlinks before copying
(setq dired-copy-dereference t)

;; try to guess target directory
(setq dired-dwim-target t)

;; adjust sorting of directories/files and print sizes in human-readable format
;; TODO: do not assume these options are supported everywhere. This will
;; probably not work on macOS and Windows.
(setq dired-listing-switches "-alhv --group-directories-first")

;; highlight files with uncommitted changes
(add-hook 'dired-mode-hook #'diff-hl-dired-mode-unless-remote)

;; truncate lines if they do not fit on the screen
(add-hook 'dired-mode-hook (lambda () (setq truncate-lines t)))

(provide 'init-dired)
