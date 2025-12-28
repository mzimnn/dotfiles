;; -*- lexical-binding: t; -*-

(defun focus-turn-on ()
  "Enable Focus mode."
  (hide-mode-line-mode)
  (olivetti-mode))

(defun focus-turn-off ()
  "Disable Focus mode."
  (hide-mode-line-mode -1)
  (olivetti-mode -1))

(define-minor-mode focus-mode
  "Focus mode."
  :lighter " Focus"
  (if focus-mode
      (focus-turn-on)
    (focus-turn-off)))

(provide 'focus-mode)
