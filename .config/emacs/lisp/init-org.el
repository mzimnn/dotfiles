;; -*- lexical-binding: t; -*-

;; support linking to man pages
(require 'ol-man)

;; do not indent coding in source code block
(setopt org-edit-src-content-indentation 0)

;; if displayable use pretty ellipsis in headings
(require 'mule-util)
(setopt org-ellipsis (truncate-string-ellipsis))

;; render quote/verse blocks like source code blocks
(setopt org-fontify-quote-and-verse-blocks t)

;; hide emphasis markers
(setopt org-hide-emphasis-markers t)

;; use relative paths for links
(setopt org-link-file-path-type 'relative)

;; allow usage of alphabetical lists
(setopt org-list-allow-alphabetical t)

;; enable Org Indent mode
(setopt org-startup-indented t)

;; customize TODO keywords
(setopt org-todo-keywords
        '((sequence "TODO(t)" "HOLD(h)" "WAIT(w)" "|" "DONE(d)" )))

;; display unlinked references in Org-roam buffer
(require 'org-roam-mode)
(add-to-list 'org-roam-mode-sections #'org-roam-unlinked-references-section t)

;; display Org-roam buffer in side window
(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-side-window)
               (side . right)
               (window-width . 0.33)
               (window-parameters . ((no-delete-other-windows . t)))))

(provide 'init-org)
