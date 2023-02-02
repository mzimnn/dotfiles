(defun mz/find-buffers-by-mode (mode)
  "Return all buffers which derive from MODE."
  (let ((buffers ()))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p mode)
          (push buffer buffers))))
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
  (insert (format-time-string (if (= arg 4) "%d.%m.%Y" "%Y-%m-%d"))))

(provide 'init-utils)
