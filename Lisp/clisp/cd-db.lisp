

(defvar *db* nil)

(defun make-cd (title artist rating ripped)
    (list :title title :artist artist :rating rating :ripped ripped))

(defun add-record (cd) (push cd *db*))

(defun dump-db ()
    (format t "~{~{~A:~10t~A~%~}~%~}" *db*))

(defun save-db (filename)
    (with-open-file (out filename
                     :direction :output
                     :if-exists :supersede)
        (with-standard-io-syntax
            (print *db* out))))

(defun load-db (filename)
    (with-open-file (in filename)
        (with-standard-io-syntax
            (setf *db* (read in)))))

(defun make-comparision-expr (field value)
    `(equal (getf cd ,field) ,value))

(defun make-comparisions-list (fields)
    (loop while fields
        collecting (make-comparision-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
    `#'(lambda (cd) (and ,@(make-comparisions-list clauses))))

(defun select (selector-fn)
     (remove-if-not selector-fn *db*))

(defun update(selector-fn &key title artist rating (ripped nil ripped-p))
    (setf *db*
        (mapcar
            #'(lambda (row)
                (when (funcall selector-fn row)
                    (if title (setf (getf row :title) title))
                    (if artist (setf (getf row :artist) artist))
                    (if rating (setf (getf row :rating) rating))
                    (if ripped-p (setf (getf row :ripped) ripped)))
                row) *db*)))

(add-record (make-cd "Roses" "Kathy Mattea" 7 t))
(add-record (make-cd "Home" "Dixie Chicks" 9 t))
(add-record (make-cd "Give Us a Break" "Limpopo" 10 t))
(add-record (make-cd "Fly" "Dixie Chicks" 8 t))


