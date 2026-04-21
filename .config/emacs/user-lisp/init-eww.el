;; -*- lexical-binding: t; -*-

;; Wrap lines after column 80
(setopt shr-max-width 80)
;; Do not render colors
(setopt shr-use-colors nil)
;; Always use monospaced fonts
(setopt shr-use-fonts nil)
;; Do not display images
(setopt shr-inhibit-images t)

;; Use lightweight frontends which EWW can render better
(setopt mz/url-replace-host-map '(("reddit.com" . "old.reddit.com")
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
  (add-to-list 'eww-url-transformers #'mz/url-replace-host))

(provide 'init-eww)
