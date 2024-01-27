(defun mz/ansi-term (&optional arg)
  "Start a terminal-emulator using bash.

If called with a prefix ARG, use a new buffer instead of reusing
the existing term buffer."
  (interactive "p")
  ;; use first buffer in list
  (let ((buffer (car (mz/find-buffers-by-mode #'term-mode))))
    (if (and buffer (not (eq arg 4)))
        (pop-to-buffer buffer)
      ;; in case of a project use its root directory
      (let ((default-directory (if (project-current)
                                   (project-root (project-current))
                                 default-directory)))
        (ansi-term "bash")))))

;; TODO: Generalize this function: It should be a higher order function which
;; excepts just MODES and returns an appropriate function.
(defun mz/derived-term-mode-p (buffer-name action)
  "Return non-nil if major mode of BUFFER-OR-NAME is derived from `term-mode'.

This function can be used as a condition in
`display-buffer-alist'. The parameter ACTION is ignored."
  (mz/derived-mode-p buffer-name 'term-mode))

(defun mz/term-handle-exit (process-name msg)
  "Kill the current buffer and delete its window.

It can be used to advice `term-handle-exit'."
  (kill-buffer)
  ;; Only delete window if it is not the only one, otherwise `delete-window'
  ;; would fail.
  (unless (one-window-p t)
    (delete-window)))

(with-eval-after-load 'term
  (keymap-set term-raw-map "M-o" nil)
  (keymap-set term-raw-map "M-x" nil))

;; open term buffer below the selected window
(add-to-list 'display-buffer-alist
             '(mz/derived-term-mode-p (display-buffer-below-selected)))

;; make URLs and email addresses clickable
(add-hook 'term-mode-hook #'goto-address-mode)

;; kill term buffer after exit
(advice-add #'term-handle-exit :after #'mz/term-handle-exit)

(provide 'init-term)
