(defun mz/find-buffers-by-mode (mode)
  "Return all buffers which derive from MODE."
  (let ((buffers ()))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p mode)
          (push buffer buffers))))
    buffers))

(defun mz/ansi-term (&optional arg)
  "Start a terminal-emulator using bash.

If called with a prefix ARG, use a new buffer instead of reusing
the existing term buffer."
  (interactive "p")
  ;; use first buffer in list
  (let ((buffer (car (mz/find-buffers-by-mode #'term-mode))))
    (if (and buffer (not (eq arg 4)))
        (display-buffer buffer)
      (ansi-term "bash"))))

(with-eval-after-load 'term
  (define-key term-raw-map (kbd "M-o") nil)
  (define-key term-raw-map (kbd "M-x") nil))

(global-set-key (kbd "C-c t") #'mz/ansi-term)

(provide 'init-term)
