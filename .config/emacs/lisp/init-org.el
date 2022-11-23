;; enable Org Indent mode
(setq org-startup-indented t)

;; hide emphasis markers
(setq org-hide-emphasis-markers t)

;; allow usage of alphabetical lists
(setq org-list-allow-alphabetical t)

;; customize TODO keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "HOLD(h)" "WAIT(w)" "|" "DONE(d)" )))

;; use relative paths for links
(setq org-link-file-path-type 'relative)

;; keybindings to invoke Org features
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)

(provide 'init-org)
