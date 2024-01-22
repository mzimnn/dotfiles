;; wrap lines after column 80
(setq shr-max-width 80)
;; do not render colors
(setq shr-use-colors nil)
;; always use monospaced fonts
(setq shr-use-fonts nil)
;; do not display images
(setq shr-inhibit-images t)

;; change default search engine in EWW
(setq eww-search-prefix "https://www.google.com/search?q=")

;; use lightweight frontends which EWW can render better
(setq mz/url-replace-host-map '(("reddit.com" . "old.reddit.com")
                                ("www.reddit.com" . "old.reddit.com")))

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

(defun mz/url-get-content-length (url)
  "Request URL and return value of header \"Content-Length\"."
  (let ((url-request-method "HEAD"))
    ;; TODO: use url-retrieve which is asynchronous
    (with-current-buffer (url-retrieve-synchronously url t)
      url-http-content-length)))

(defun mz/url-display-content-length (url)
  "Display content length of URL in echo area."
  (interactive "sURL: ")
  (let ((len (mz/url-get-content-length url)))
    (message (if len
                 (file-size-human-readable-iec len)
               "Unknown size"))))

(defun mz/eww-display-content-length ()
  "In EWW, display content length of url under point."
  (interactive)
  (mz/url-display-content-length (shr-url-at-point nil)))

(with-eval-after-load 'eww
  ;; change key bindings in eww mode
  (keymap-set eww-mode-map "n" #'next-line)
  (keymap-set eww-mode-map "p" #'previous-line)
  (keymap-set eww-mode-map "N" #'eww-next-url)
  (keymap-set eww-mode-map "P" #'eww-previous-url)
  (keymap-set eww-mode-map "V" #'eww-view-source)
  (keymap-set eww-mode-map "v" nil)
  (keymap-set eww-mode-map "M-s" #'mz/eww-display-content-length)

  ;; change key bindings in eww bookmarks mode
  (keymap-set eww-bookmark-mode-map "n" #'next-line)
  (keymap-set eww-bookmark-mode-map "p" #'previous-line))

(provide 'init-eww)
