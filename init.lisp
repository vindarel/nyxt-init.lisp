#| @section My Next browser init file

- @link{https://github.com/next-browser/next}{Next browser}
- @link{https://github.com/atlas-engineer/next/blob/master/documents/MANUAL.org}{Next manual.org}

We're writing it in literate programing with @link{https://github.com/mmontone/erudite}{erudite}.

---
The command required to produce the readme is:

@verbatim
(erudite:erudite #p"README.md" "init.lisp" :output-type :markdown :syntax :erudite)
@end verbatim

About Erudite:

We currently use the erudite syntax and produce markdown output. It
will be possible in the next version (or now if you add a missing piece,
see the issues) to use markdown for input and output (but probably not
to produce other output).

A @emph{C-c ~} in Slime is useful to sync the package and directory.

---
If not specified, all code is written in the next package.
|#

(in-package :next)

;; @section Commands configuration
;; @subsection Open files with my preferred program

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

;; @subsection Open the home directory in the browser to see all files (file://)
;; from @link{https://github.com/tviti/next-cfg/}{https://github.com/tviti/next-cfg/}

(define-command home-directory ()
  "Open my home directory in a browser window (useful for viewing html exports
e.g. from org-mode or an Rmarkdown doc)."
  (let ((url (concatenate 'string "file://"
			  (directory-namestring (truename "~/")))))
    (set-url url)))

;; @subsection Git cloner
(setf next/vcs:*vcs-projects-roots* '("~/projets"
                                      "~/work"
                                      "~/bacasable/lisp-projects"
                                      "~/common-lisp"
                                      "~/quicklisp/local-projects"))

;; My default username.
;; We can also set *vcs-usernames-alist* for github.com and other domains.
(setf next/vcs:*vcs-username* "vindarel")

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
;; For *you* who shouldn't use Facebook ;)
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

;; @subsubsection FIP radio

;; The @link{https://www.fip.fr/}{fip radio} is great: eclectic music without any ads.
;; We developed the following commands:
;; - fip-save-current-song to save the currently playing song to disk
;; - fip-view-saved-songs to show them all in a new buffer (with a link to a youtube search)
;; - fip-listen-saved-song to fuzzy-select a song and search it on youtube (where we can use `download-video`).

;; Save the current playing song's title into a text file.
;; Caution: it seems the website can lag to a couple minutes behind the music :S At least we also save the current time, so we could go back search for the title.

(defparameter *fip-home-url* "https://www.fip.fr/")

(defparameter *fip-database-file* #p"~/.config/next/fip.txt"
              "Where to save our songs.")

;; Quickloading a library adds an extra startup time.
;; At some point we'll want to create a Next binary with those libraries.
(ql:quickload '("lquery"
                ;; access is a library for consistent and nested access to all data structures.
                "access"
                "cl-punch"
                ))

(defun fip-current-song ()
  "Return the artist, the song, the year (multiple values)."
  (handler-case
      (let* ((html (dex:get *fip-home-url*))
             (parsed (lquery:$ (initialize html)))
             (title (access:access ;; get first elt of vector, don't fail if out of bounds.
                     (lquery:$ parsed ".now-info-title" (text)) ; returns a vector.
                     0))
             (artist/year (access:access
                           (lquery:$ parsed ".now-info-subtitle" (text))
                           0))
             (matches (when artist/year
                        ;; returns a vector with the matched strings.
                        ;; see the Cookbook.
                        (nth-value 1 (ppcre:scan-to-strings "\(.*\) \((.*\))" artist/year))))
             (artist (when matches
                       (access:access matches
                                      0)))
             (year (when matches
                     (access:access matches
                                    1))))
        (echo "FIP: artist: ~a, song ~a - ~a~&" artist title year)
        (values artist title year))
    (error (c)
      (echo-warning t "Getting FIP current song failed: ~a~&" c))))

(define-command fip-save-current-song ()
  "Save the current song title, artist and year of the album playing on fip on file."
  (multiple-value-bind (artist title year)
      (fip-current-song)
    (if artist
        (with-open-file (f *fip-database-file*
                           :direction :output
                           :if-does-not-exist :create
                           :if-exists :append)
          (let ((*print-pretty* nil))
            ;; Pretty printing will insert a line break at around 80 characters,
            ;; between :date and the date, making it un-readable with uiop:read-file-forms.
            (format f "~s~&" (list :artist artist :song title :year year
                                   :date (local-time:format-timestring nil (local-time:now))))))
        (echo "fip: no artist found."))))

(defun song-alist2html (alist)
  "From an alist with :artist, :title and all, return html."
  (format nil "~a, ~a ~a"
          (access:access alist :artist)
          (access:access alist :song)
          (access:access alist :year)))

(defun list2html (list)
  (cl-markup:markup
   (:head)
   (:body
    (:h1 "fip songs")
    (:ul
     (loop for elt in list
        collect (cl-markup:markup (:li (:span (song-alist2html elt))
                                       (:span "  "
                                              (:a :href (format nil "https://www.youtube.com/results?search_query=~a+~a"
                                                                (access:access elt :artist)
                                                                (access:access elt :song))
                                                  "youtube ⮱")))))))))

(define-command fip-view-saved-songs ()
  "Show the fip songs we saved, along with a youtube search link."
  (let ((buffer (find-if (lambda (b)
                           (string= "*fip*" (title b)))
                         (alexandria:hash-table-values (buffers *interface*))))
        (forms (uiop:read-file-forms *fip-database-file*)))
    (unless buffer
      (setf buffer (make-buffer
                    :title "*fip*"
                    :modes (cons 'help-mode
                                 (get-default 'buffer 'default-modes)))))
    (let* ((content (list2html forms))
           (insert-content (ps:ps (setf (ps:@ document body |innerHTML|)
                                        (ps:lisp content)))))
      (rpc-buffer-evaluate-javascript buffer insert-content))
    (set-current-buffer buffer)
    buffer))

(defun fip-print-saved-songs (&key verbose)
  "Print the fip songs we saved."
  (handler-case
      (let ((forms (uiop:read-file-forms *fip-database-file*)))
        ;XXX: table output with vindarel/cl-ansi-term ?
        (mapcar (lambda (it)
                  (format t "~a - ~a ~a~&" (second it) (fourth it) (sixth it)))
                forms))
    (error (c)
      (echo-warning "Error reading our fip songs file: ~&" c)
      (format t "Error reading our fip songs file: ~a~&" c))))

(defun songs-completion-filter (input)
  (let* ((forms (uiop:read-file-forms *fip-database-file*))
         (strings (mapcar (lambda (elt)
                            (format nil "~a - ~a"
                                    (access:access elt :artist)
                                    (access:access elt :song)))
                          forms)))
    (fuzzy-match input strings)))

(define-command fip-listen-saved-song ()
  "Pick one song that was saved on disk, and search it on Youtube."
  (with-result (selection (read-from-minibuffer
                           (make-instance 'minibuffer
                                          :default-modes '(minibuffer-mode)
                                          :input-prompt "pick a song"
                                          :completion-function #'songs-completion-filter)))

    (format t "~&choice: ~a~&" selection)
    (set-url-to-buffer (format nil  "https://www.youtube.com/results?search_query=~a" selection)
                       :new-buffer-p t)) )
