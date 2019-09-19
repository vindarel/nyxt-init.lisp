(in-package :next)

(defun my-open-videos (filename)
  "Open videos with mpv."
  (handler-case (let ((extension (pathname-type filename)))
                  (match extension
                    ((or "flv" "mkv" "mp4")
                     (uiop:launch-program (list "mpv" filename)))
                    (_
                     (next/file-manager-mode:open-file-function filename))))
    (error (c) (log:error "Error opening pdf ~a: ~a" filename c))))

(setf next/file-manager-mode:*open-file-function* #'my-open-videos)

(defun old-reddit-hook (url)
  "Enforce old reddit."
  (let* ((uri (quri:uri url)))
    (if (search "www.reddit" (quri:uri-host uri))
        (progn
          (setf (quri:uri-host uri) "old.reddit.com")
          (let ((new-url (quri:render-uri uri)))
            (log:info "Switching to old Reddit: ~a" new-url)
            new-url))
        url)))
(add-to-default-list #'old-reddit-hook 'buffer 'load-hook)

(defun no-facebook-hook (url)
  "Always redirect to Diaspora."
  (let ((uri (quri:uri url)))
    (if (search "facebook.com" (quri:uri-host uri))
        (progn
          (setf (quri:uri-host uri) "pod.geraspora.de")
          (let ((new-url (quri:render-uri uri)))
            (log:info "You shall not go to Facebook!" new-url)
            new-url))
        url)))
(add-to-default-list #'no-facebook-hook 'buffer 'load-hook)
