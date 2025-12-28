;; -*- lexical-binding: t; -*-

;; Support linking to man pages
(require 'ol-man)

;; Do not indent coding in source code block
(setopt org-edit-src-content-indentation 0)

;; If displayable use pretty ellipsis in headings
(require 'mule-util)
(setopt org-ellipsis (truncate-string-ellipsis))

;; Render quote/verse blocks like source code blocks
(setopt org-fontify-quote-and-verse-blocks t)

;; Hide emphasis markers
(setopt org-hide-emphasis-markers t)

;; Use relative paths for links
(setopt org-link-file-path-type 'relative)

;; Allow usage of alphabetical lists
(setopt org-list-allow-alphabetical t)

;; Enable Org Indent mode
(setopt org-startup-indented t)

;; Customize TODO keywords
(setopt org-todo-keywords
        '((sequence "TODO(t)" "HOLD(h)" "WAIT(w)" "|" "DONE(d)" )))

;; Display unlinked references in Org-roam buffer
(require 'org-roam-mode)
(add-to-list 'org-roam-mode-sections #'org-roam-unlinked-references-section t)

;; Display Org-roam buffer in side window
(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-side-window)
               (side . right)
               (window-width . 0.33)
               (window-parameters . ((no-delete-other-windows . t)))))

(provide 'init-org)
