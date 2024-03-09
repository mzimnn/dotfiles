;; do not indent coding in source code block
(setq org-edit-src-content-indentation 0)

;; render quote/verse blocks like source code blocks
(setq org-fontify-quote-and-verse-blocks t)

;; hide emphasis markers
(setq org-hide-emphasis-markers t)

;; use relative paths for links
(setq org-link-file-path-type 'relative)

;; allow usage of alphabetical lists
(setq org-list-allow-alphabetical t)

;; enable Org Indent mode
(setq org-startup-indented t)

;; customize TODO keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "HOLD(h)" "WAIT(w)" "|" "DONE(d)" )))

(provide 'init-org)
