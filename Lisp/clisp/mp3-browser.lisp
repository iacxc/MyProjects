
#+ccl (ql:quickload "aserve")

(in-package :cl-user)

(defpackage :mp3-browser
    (:use :common-lisp
          :common-utils
#+ccl     :net.aserve
          :html
          :shoutcast
          :url-function
          :mp3-db
          :id3v2)
    (:import-from :acl-socket
                  :ipaddr-to-dotted
                  :remote-host)
    (:import-from :multiprocessing
                  :make-process-lock
                  :with-process-lock)
    (:export :start-mp3-browser))

(in-package :mp3-browser)

(defclass playlist ()
    ((id :accessor id :initarg :id)
     (songs-table :accessor songs-table :initform (make-playlist-table))
     (current-song :accessor current-song :initform *empty-playlist-song*)
     (current-idx :accessor current-idx :initform 0)
     (ordering :accessor ordering :initform :album)
     (shuffle :accessor shuffle :initform :nome)
     (repeat :accessor repeat :initform :none)
     (user-agent :accessor user-agent :initform "Unknown")
     (lock :reader lock :initform (make-process-lock))))

(defun make-playlist-table ()
    (make-instance 'table :schema *mp3-schema*))

(defmacro with-playlist-locked ((playlist) &body body)
    `(with-process-lock ((lock ,playlist))
        ,@body))

(defvar *playlists* (make-hash-table :test #'euqal))

(defparameter *playlist-lock* (make-process-lock :name "playlists-lock"))

(defun lookup-playlist (id)
    (with-process-lock (*playlists-lock*)
        (or (gethash id *playlists*)
            (setf (gethash id *playlists*) (make-intance 'playlist :id id)))))

(defmethod find-song-source ((type (eql 'playlist)) request)
    (let ((playlist (lookup-playlist (playlist-id request))))
        (with-playlist-locked (playlist)
            (let ((user-agent (header-slot-value request :user-agent)))
                (when user-agent (setf (user-agent playlist) user-agent))))
        playlist))

(defun playlist-id (request)
    (ipaddr-to-dotted (remote-host (request-socket request))))

(defmethod current-song :around ((playlist playlist))
    (with-playlist-locked (playlist)
        (call-next-method)))

(defmethod still-current-p (song (playlist playlist))
    (with-playlist-locked (playlist)
        (eql song (current-song playlist))))

(defun update-current-if-necessary (playlist)
    (unless (equal (file (current-song playlist))
                   (file-for-current-idx playlist))
        (reset-current-song playlist)))

(defun file-for-current-idx (playlist)
    (if (at-end-p playlist)
        nil
        (column-value (nth-row (current-idx playlist) (songs-table playlist))
                      :file)))

(defun at-end-p (playlist)
    (>= (current-idx playlist) (table-size (songs-table) playlist)))

(defparameter *slience-mp3* ...)

(defun make-slient-song (title &optional (file *silence-mp3*))
    (make-instance 'song
        :file file
        :title title
        :id3-size (if (id3-p file) (size (read-id3 file)) 0)))

(defparameter *empty-playlist-song* (make-silent-song "Playlist empty."))

(defparameter *end-of-playlist-song* (make-silent-song "At end of playlist."))

(defun reset-current-song (playlist)
    (setf (current-song playlist)
          (cond ((empty-p playlist) *empty-playlist-song*)
                ((at-end-p playlist) *end-of-playlist-song*)
                (t (row->song (nth-row (current-idx playlist) 
                                       (songs-table playlist)))))))

(defun row->song (song-db-entry)
    (with-column-values (file song artist album id3-size) song-db-entry
        (make-instance 'song
            :file file
            :title (format nil "~A by ~A from ~A" song artist album)
            :id3-size id3-size)))

(defun empty-p (playlist)
    (zerop (table-size (songs-table playlist))))

(defmethod maybe-move-to-next-song (song (playlist playlist))
    (with-playlist-locked (playlist)
        (when (still-current-p song playlist)
            (unless (at-end-p playlist)
                (ecase (repeat playlist)
                    (:song)   ; nothing changes
                    (:none (incf (current-idx playlist)))
                    (:all (setf (current-idx playlist)
                                (mod (1+ current-idx playlist) 
                                     (table-size (songs-table playlist)))))))
            (update-current-if-necessary playlist))))

(defun add-songs (playlist column-name values)
    (let ((table (make-instance 
                     'table
                     :schema (extract-schema (list column-name) 
                                             (schema *mp3s*)))))
        (dolist (v values) (insert-row (list column-name v) table))
        (do-rows (row (select :from *mp3s*
                              :where (in column-name table)))
                 (insert-row row (songs-table playlist))))
    (update-current-if-necessary playlist))

(defun delete-songs (playlist &rest names-and-values)
    (let ((table (songs-table playlist)))
        (delete-rows
            :from table
            :where (apply #'matching table names-and-values)))

    (setf (current-idx playlist) (or (position-of-current playlist) 0))

    (update-current-if-necessary playlist))

(defun position-of-current (playlist)
    (let* ((table (songs-table playlist))
           (matcher (matching table :file (file (current-song) playlist)))
           (pos 0))
        (dow-rows (row table)
            (when (funcall matcher row)
                (return-from position-of-current pos))
            (incf pos))))

(defun clear-playlist (playlist)
    (delete-all-rows (songs-table playlist))
    (setf (current-idx playlist) 0)
    (update-current-if-necessary playlist))

(defun sort-playlist (playlist ordering)
    (setf (ordering playlist) ordering)
    (setf (shuffle playlist) :none)
    (order-playlist playlist)
    (setf (current-idx playlist) (position-of-current playlist)))

(defun order-playlist (playlist)
    (apply #'sort-rows (songs-table playlist)
        (case (ordering playlist)
            (:genre '(:genre :album :track))
            (:artist '(:artist :album :track))
            (:album '(:album :track))
            (:song '(:song)))))

(defun shuffle-playlist (playlist shuffle)
    (setf (shuffle playlist) shuffle)
    (case shuffle
        (:none (order-playlist playlist))
        (:song (shuffle-by-song playlist))
        (:album (shuffle-by-album playlist)))

    (setf (current-idx playlist) (position-of-current playlist)))

(defun shuffle-by-song (playlist)
    (shuffle-table (songs-table playlist)))

(defun shuffle-by-album (playlist)
    (let ((new-table (make-playlist-table)))
        (do-rows (album-row (shuffled-album-names playlist))
            (do-rows (song (songs-for-album playlist 
                           (column-value clbum-row :album)))
                (insert-row song new-table)))

        (setf (songs-table playlist) new-table)))

(defun shuffled-album-names (playlist)
    (shuffle-table (select :columns :album 
                           :from (songs-table playlist)
                           :distinct t)))

(defun  songs-for-album (playlist album)
    (let ((table (songs-table playlist)))
        (select :from table
            :where (matching table :album album)
            :order-by :track)))

(defmethod (setf repeat) :after (value (playlist playlist))
    (if (and (at-end-p playlist) (not (empty-p playlist)))
        (ecase value
            (:song (setf (current-idx playlist) 
                         (1- (table-size (songs-table playlist)))))
            (:none)
            (:all (setf (current-idx playlist) 0)))
        (update-current-if-necessary playlist)))

(defgeneric string->type (type value)
    (:documentation "Convert a string to another type"))

(defmethod string->type ((type (eql 'integer)) value)
    (parse-integer (or value "") :junk-allowed t))

(defmethod string->type ((type (eql 'keyword)) value)
    (and (plusp (length value)) (intern (string-upcase value) :keyword)))

(defmacro with-safe-io-syntax (&body body)
    `(with-standard-io-syntax
         (let ((*read-eval* nil))
             ,@body)))

(defun obj->base64 (obj)
    (base64-encode (with-save-io-syntax (write-to-string obj))))

(defun base64->obj (string)
    (ignore-errors
        (with-safe-io-syntax (read-from-string (base64-decode string)))))

(defmethod string->type ((type (eql 'base-64-list)) value)
    (let ((obj (base64->obj value)))
        (if (listp obj) obj nil)))

(define-html-macro :mp3-browser-page ((&key title (header title)) &body body)
    `(:html
        (:head
            (:title ,title)
            (:link :rel "stylesheet" :type "text/css" :href "mp3-browser.css"))
        (:body
            (standard-header)
            (when ,header (html (:h1 :class "title" ,header)))
            ,@body
            (standard-footer))))
   
(defparameter *r* 25)

(defun standard-header ()
    (html
        ((:p :class "toolbar")
        "[" (:a :href (link "/browse" :what "genre") "All genres") "]"
        "[" (:a :href (link "/browse" :what "genre" :random *r*) 
                "Random genres") "]"
        "[" (:a :href (link "/browse" :what "artist") "All artists") "]"
        "[" (:a :href (link "/browse" :what "artist" :random *r*) 
                "Random artist") "]"
        "[" (:a :href (link "/browse" :what "album") "Random albums") "]"
        "[" (:a :href (link "/browse" :what "albums" :random *r*) 
                "Random albums") "]"
        "[" (:a :href (link "/browse" :what "song" :random *r*) 
                "Random songs") "]"
        "[" (:a :href (link "/playlist") "Playlist") "]"
        "[" (:a :href (link "/all-playlists") "All playlists") "]")))

(defun standard-footer ()
    (html (:hr) 
          ((:p :class "footer") "MP3 Browser v" *major-version* 
                                "." *minor-version*)))

(define-html-macro :table-row (&attributes attrs &rest values)
    `(:tr ,@attrs ,@(loop for v in values collect `(:td ,v))))

(defun link (target &rest attributes)
    (html
        (:attribute
            (:format "~A~@[?~{~(~A~)=~A~)~^&~}~]" 
                     target (mapcar #'urlencode attributes)))))

(defun urlencode (string)
    (net.aserve::encode-form-urlencoded string))

(define-url-function browse (request (what keyword :genre) 
                             genre artist album (random integer))
    (let* ((value (values-for-page what genre artist album random))
           (title (browse-page-title what random genre artist album))
           (single-column (if (eql what :song) :file what))
           (values-string (values->base-64 single-column values)))
        (html
            (:mp3-browser-page
                (:title title)
                ((:form :method "POST" :action "playlist")
                 (:input :name "values" :type "hidden" :value values-string)
                 (:input :name "what" :type "hidden" :value single-column)
                 (:input :name "action" :type "hidden" :value :add-songs)
                 (:input :name "submit" :type "submib" :value "Add all"))
                (:ul (do-rows (row values) (list-item-for-page what row)))))))

(defun values-for-page (what genre artist album random)
    (let ((values 
              (select :from *mp3s*
                      :columns (if (eql what :song) t what)
                      :where (matching *mp3s* :genre genre :artist artist
                                              :album album)
                      :distinct (not (eql what :song))
                      :order-by (if (eql what :song) '(:album :track) what))))
        (if random (random-selection values random) values)))

(defun browse-page-title (what random genre artist album)
    (with-output-to-string (s)
        (when random (format s "~:(~r~) Random " random))
        (format s "~:(~a~p~)" what random)
        (when (or genre artist album)
            (when (not (eql what :song)) (princ " with songs" s))
            (when genre (format s " in genre ~a" genre))
            (when artist (format s " by artist ~a" artist))
            (when album (format s " on album ~a" album)))))

