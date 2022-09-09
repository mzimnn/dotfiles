;; wrap lines after column 80
(setq shr-max-width 80)
;; do not render colors
(setq shr-use-colors nil)
;; always use monospaced fonts
(setq shr-use-fonts nil)
;; do not display images
(setq shr-inhibit-images t)

;; use lightweight frontends which EWW can render better
(setq mz/url-replace-host-map '(("reddit.com" . "libredd.it")
                                ("old.reddit.com" . "libredd.it")
                                ("www.reddit.com" . "libredd.it")))

(defun mz/url-replace-host (url)
  "Replace host of URL according to `mz/url-replace-host-map'."
  (let* ((parsed-url (url-generic-parse-url url))
         (host (url-host parsed-url))
         (host-replacement
          (alist-get host mz/url-replace-host-map nil nil #'string-equal)))
    (when host-replacement
      (setf (url-host parsed-url) host-replacement))
    (url-recreate-url parsed-url)))

;; ensure host is correctly displayed while loading the page
(advice-add 'eww--dwim-expand-url :filter-return #'mz/url-replace-host)
;; ensure redirects are considered
(advice-add 'url-retrieve-internal :filter-args
            (lambda (args)
              (let ((url (mz/url-replace-host (car args)))
                    (status (car (caddr args))))
                (when (plist-get status :redirect)
                  (plist-put status :redirect url))
                (cons url (cdr args))))
            '((name . "mz/url-replace-host")))

;; change keybindings in eww mode
(add-hook 'eww-mode-hook
          (lambda ()
            (local-set-key "n" 'next-line)
            (local-set-key "p" 'previous-line)
            (local-set-key "N" 'eww-next-url)
            (local-set-key "P" 'eww-previous-url)
            (local-set-key "V" 'eww-view-source)
            (local-unset-key "v")))

;; change keybindings in eww bookmarks mode
(add-hook 'eww-bookmark-mode-hook
          (lambda ()
            (local-set-key "n" 'next-line)
            (local-set-key "p" 'previous-line)))

(provide 'init-eww)
