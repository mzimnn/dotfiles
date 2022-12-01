(defun mz/derived-mode-p (buffer-or-name modes)
  "Return non-nil if major mode of BUFFER-OR-NAME is derived from one of MODES."
  (with-current-buffer buffer-or-name
    (derived-mode-p modes)))

(defun mz/find-buffers-by-mode (mode)
  "Return all buffers which derive from MODE."
  (let ((buffers ()))
    (dolist (buffer (buffer-list))
      (when (mz/derived-mode-p buffer mode)
        (push buffer buffers)))
    buffers))

(defun mz/other-window-backwards ()
  "Like `other-window' but in reverse."
  (interactive)
  (other-window -1))

(defun mz/insert-current-date (&optional arg)
  "Insert current date into buffer.

If called with a prefix ARG, use European format of date."
  (interactive "p")
  (when (use-region-p)
    (delete-region (region-beginning) (region-end)))
  (insert (format-time-string (if (= arg 4) "%d.%m.%Y" "%Y-%m-%d")))
  (when (eq (bound-and-true-p evil-state) 'normal)
    (evil-insert-state)))

(defun mz/sort-alphabetically (strings)
  "Sort STRINGS in alphabetical order."
  (sort strings (lambda (str1 str2)
                  (string-lessp (downcase str1) (downcase str2)))))

(defun mz/ispell-program-installed-p ()
  "Check if `ispell-program-name' is installed."
  (require 'ispell)
  (when (executable-find ispell-program-name) t))

(defun mz/comment-line ()
  "Un-/comment current line or each line in the active region."
  (interactive)
  (let ((begin (if (use-region-p) (region-beginning) (line-beginning-position)))
        (end (if (use-region-p) (region-end) (line-end-position))))
    (comment-or-uncomment-region begin end)))

(defun mz/display-json-from-clipboard ()
  "Prettify JSON from clipboard and display it."
  (interactive)
  (message "Reading JSON from clipboard...")
  (let ((inhibit-message t)
        (buffer (generate-new-buffer "*json*")))
    (with-current-buffer buffer
      (clipboard-yank)
      (json-pretty-print-buffer)
      (js-json-mode))
    (pop-to-buffer buffer))
  (message "Reading JSON from clipboard...done"))

(provide 'init-utils)
