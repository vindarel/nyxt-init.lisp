#| @section My Next browser init file

https://github.com/next-browser/next

https://github.com/atlas-engineer/next/blob/master/documents/MANUAL.org

We're writing it in literate programing with @link{https://github.com/mmontone/erudite}{erudite}.

If not specified, all code is written in the next package.|#

(in-package :next)

;; @section Commands configuration
;; @subsection Open files with my preferred program (videos with mpv)

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

@subsection Git cloner
(setf next/vcs:*vcs-projects-roots* '("~/projets"
                                      "~/work"
                                      "~/bacasable/lisp-projects"
                                      "~/common-lisp"
                                      "~/quicklisp/local-projects"))

;; @section Hooks
;; @subsection Old reddit hook

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

;; @subsection Facebook to Diaspora hook
;; Not that I'm using Facebook :D
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

#|
@subsection Unused hooks
These  are  working©,  but  I  don't  use  them  (they're  either  too
revolutionnary either too dumb :p ).
|#

#|
@subsubsection Automatically download Youtube videos
|#

(defun auto-yt-dl (url)
  "Download this url asynchronously to /tmp/videos, according youtube-dl is installed globally."
  (when (search "www.youtube.com" url)
    (log:info "Youtube: downloading ~a" url)
    (uiop:launch-program (list "youtube-dl" url "-o" "/tmp/videos/%(title)s.%(ext)s")))
  url)
(format t "when finished, do: add-to-default-list #'auto-yt-dl 'buffer 'load-hook")
