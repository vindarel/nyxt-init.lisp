
# My Next browser init file


https://github.com/next-browser/next

https://github.com/atlas-engineer/next/blob/master/documents/MANUAL.org

We're writing it in literate programing with [erudite](https://github.com/mmontone/erudite).

The command required to produce the readme is:

(erudite:erudite #p"README.md" "init.lisp" :output-type :markdown :syntax :erudite)
If not specified, all code is written in the next package.

```lisp

(in-package :next)

```

# Commands configuration


## Open files with my preferred program


```lisp

(defun my-open-files (filename)
  "Open videos with mpv, directories with emacsclient."
  (let ((args)
        (extension (pathname-type filename)))
    (cond
      ((uiop:directory-pathname-p filename)
       (log:info "Opening ~a with emacsclient." filename)
       (setf args (list "emacsclient" filename)))

      ((member extension '("flv" "mkv" "mp4") :test #'string-equal)
       (setf args (list "mpv" filename))))

    (handler-case (if args
                      (uiop:launch-program args)
                      (next/file-manager-mode:open-file-function filename))
      (error (c) (log:error "Error opening ~a: ~a" filename c)))))

(setf next/file-manager-mode:*open-file-function* #'my-open-files)

```

## Open the home directory in the browser to see all files (file://)

from [https://github.com/tviti/next-cfg/](https://github.com/tviti/next-cfg/)

```lisp

(define-command open-home-dir ()
  "Open my home directory in a browser window (useful for viewing html exports
e.g. from org-mode or an Rmarkdown doc)."
  (let ((url (concatenate 'string "file://"
			  (directory-namestring (truename "~/")))))
    (set-url url)))

```

## Git cloner


```lisp
(setf next/vcs:*vcs-projects-roots* '("~/projets"
                                      "~/work"
                                      "~/bacasable/lisp-projects"
                                      "~/common-lisp"
                                      "~/quicklisp/local-projects"))

```
My default username.
We can also set *vcs-usernames-alist* for github.com and other domains.

```lisp
(setf next/vcs::*vcs-username* "vindarel")

```

# Hooks


## Old reddit hook


```lisp

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

```

## Facebook to Diaspora hook

For *you* who shouldn't use Facebook ;)

```lisp
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

```


## Unused hooks

These  are  working©,  but  I  don't  use  them  (they're  either  too
revolutionnary either too dumb :p ).


### Automatically download Youtube videos


```lisp

(defun auto-yt-dl (url)
  "Download this url asynchronously to /tmp/videos, according youtube-dl is installed globally."
  (when (search "www.youtube.com" url)
    (log:info "Youtube: downloading ~a" url)
    (uiop:launch-program (list "youtube-dl" url "-o" "/tmp/videos/%(title)s.%(ext)s")))
  url)
(format t "when finished, do: add-to-default-list #'auto-yt-dl 'buffer 'load-hook")
```
