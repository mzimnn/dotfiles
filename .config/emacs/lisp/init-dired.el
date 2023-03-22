;; dereference symlinks before copying
(setq dired-copy-dereference t)

;; try to guess target directory
(setq dired-dwim-target t)

;; adjust sorting of directories/files and print sizes in human-readable format
;; TODO: do not assume these options are supported everywhere. This will
;; probably not work on macOS and Windows.
(setq dired-listing-switches "-alhv --group-directories-first")

(defun mz/configure-dired-mode ()
  "Configure `dired-mode'."
  ;; truncate lines if they do not fit on the screen
  (setq truncate-lines t)
  ;; highlight files with uncommitted changes
  (diff-hl-dired-mode-unless-remote))

(add-hook 'dired-mode-hook #'mz/configure-dired-mode)

(provide 'init-dired)
