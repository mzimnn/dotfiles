;; -*- lexical-binding: t; -*-

;; Dereference symbolic links before copying
(setopt dired-copy-dereference t)

;; Try to guess target directory
(setopt dired-dwim-target t)

;; Adjust sorting of directories/files and print sizes in human-readable format
;; TODO: do not assume these options are supported everywhere. This will
;; probably not work on macOS and Windows.
(setopt dired-listing-switches "-alhv --group-directories-first")

(defun mz/configure-dired-mode ()
  "Configure `dired-mode'."
  ;; Truncate lines if they do not fit on the screen
  (setq-local truncate-lines t)
  ;; Automatically update Dired buffer if something changed on the file system
  (auto-revert-mode)
  ;; Highlight files with uncommitted changes
  (diff-hl-dired-mode-unless-remote))

(add-hook 'dired-mode-hook #'mz/configure-dired-mode)

(provide 'init-dired)
