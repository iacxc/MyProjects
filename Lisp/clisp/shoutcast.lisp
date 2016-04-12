
;;;; Shoutcast.lisp

#+ccl (ql:quickload "aserve")
(load "id3v2")

(in-package :cl-user)

(defpackage :shoutcast
    (:use :common-lisp
          :common-utils
#+ccl     :net.aserve
          :id3v2)
    (:export :song
             :file
             :id3-size
             :find-song-source
             :current-song
             :still-current-p
             :maybe-move-to-next-song
             :*song-source-type*))

(in-package :shoutcast)

(defgeneric current-song (source)
    (:documentation "Return the currently playing song or NIL."))

(defgeneric maybe-move-to-next-song (song ource)
    (:documentation "If the give sone is still the current one update
                     the value returned by the current-song."))

(defgeneric still-current-p (song source)
    (:documentation "Return true if the song give is the same as the 
                     current-song."))

(defgeneric find-song-source (type request)
    (:documentation "Find the song-source of the given type for the 
                     given request."))
 
(defclass song ()
    ((file     :reader file     :initarg :file)
     (title    :reader title    :initarg :title)
     (id3-size :reader id3-size :initarg :id3-size)))

(defclass simple-song-queue ()
    ((songs :accessor songs
            :initform (make-array 10 :adjustable t :fill-pointer 0))
     (index :accessor index
            :initform 0)))

(defparameter *songs* (make-instance 'simple-song-queue))

(defmethod find-song-source ((type (eql 'singleton)) request)
    (declare (ignorable request))
    *songs*)

(defmethod current-song ((source simple-song-queue))
    (when (array-in-bounds-p (songs source) (index source))
        (aref (songs source) (index source))))

(defmethod still-current-p (song (source simple-song-queue))
    (eql song (current-song source)))

(defmethod maybe-move-to-next-song (song (source simple-song-queue))
    (when (still-current-p song source)
        (incf (index source))))

(defun file->song (file)
    (let ((id3 (read-id3 file)))
        (make-instance 'song
            :file (namestring (truename file))
            :title (format nil "~A by ~A from ~A" 
                       (song id3) (artist id3) (album id3))
            :id3-size (size id3))))

(defun add-file-to-songs (file)
    (vector-push-extend (file->song file) (songs *songs*)))

(defparameter *timeout-seconds* (* 60 60 24 365 10))

(defun shoutcast (request entity)
    (with-http-response 
        (request entity :content-type "audio/MP3" :timeout *timeout-seconds*)
        (prepare-icy-response request *metadata-interval*)
        (let ((wants-metadata-p (header-slot-value request :icy-metadata)))
            (with-http-body (request entity)
                (play-songs (request-socket request)
                            (find-song-source *song-source-type* request)
                            (if wants-metadata-p *metadata-interval*))))))

(defun prepare-icy-response (request metadata-interval)
    (setf (request-reply-protocol-string request) "ICY")
    (loop for (k v) in (reverse 
            `((:|icy-metaint| ,(any->string metadata-interval))
              (:|icy-notice1| "<BR> This stream blah blah blah<BR>")
              (:|icy-notice2| "More blah")
              (:|icy-name| "My Lisp Shoutcast Server")
              (:|icy-genre| "Unknown")
              (:|icy-url| ,(request-uri request))
              (:|icy-pub| "1")))
        do (setf (reply-header-slot-value request k) v))
    (turn-off-chunked-transfer-encoding request))

(defun turn-off-chunked-transfer-encoding (request)
    (setf (request-reply-strategy request)
        (remove :chunked (request-reply-strategy request))))



(show-exports *package*)

