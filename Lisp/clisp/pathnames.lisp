
(in-package :cl-user)

(defpackage :pathnames
    (:use :common-lisp
          :common-utils)
    (:export
        :list-directory
        :file-exists-p
        :pathname-as-directory
        :pathname-as-file
        :walk-directory
        :directory-p
        :file-p))

(in-package :pathnames)

(defun component-present-p (value)
    (and value (not (eql value :unspecific))))

(defun directory-p (p)
    (and (not (component-present-p (pathname-name p)))
         (not (component-present-p (pathname-type p)))
         p))

(defun file-p (f)
    (and f (or (component-present-p (pathname-name f))
               (component-present-p (pathname-type f)))))

(defun pathname-as-directory (name)
    (let ((pathname (pathname name)))
        (when (wild-pathname-p pathname)
            (error "Can't relably convert wild pathnames."))
        (if (not (directory-p name))
            (make-pathname
                :directory (append (or (pathname-directory pathname)
                                       (list :relative))
                                   (list (file-namestring pathname)))
                :name nil
                :type nil
                :defaults pathname)
            pathname)))

(defun directory-wildcard (dirname)
    (make-pathname
        :name :wild
        :type #-clisp :wild #+clisp nil
        :defaults (pathname-as-directory dirname)))

#+clisp
(defun clisp-subdirectories-wildcard (wildcard)
    (make-pathname
       :directory (append (pathname-directory wildcard) (list :wild))
       :name nil
       :type nil
       :defaults wildcard))

(defun list-directory (dirname)
    (when (wild-pathname-p dirname)
        (error "Can only list concrete directory names."))
    (let ((wildcard (directory-wildcard dirname)))
        #+(or sbcl cmu lispworks) (directory wildcard)

        #+openmcl (directory wildcard :directories t)

        #+allegro (directory wildcard :directories-are-files nil)

        #+clisp (nconc (directory wildcard)
                       (directory (clisp-subdirectories-wildcard wildcard)))

        #-(or sbcl cmu lispworks openmcl allegro clisp)
        (error "list-directory not implemented")))

(defun file-exists-p (pathname)
    #+(or sbcl lispworks openmcl) (probe-file pathname)
    #+(or allegro cmu) (or (probe-file (pathname-as-directory pathname))
                           (probe-file pathname))
    #+clisp
    (or (ignore-errors
            (probe-file (pathname-as-file pathname)))
        (ignore-errors
            (let ((directory-form (pathname-as-directory pathname)))
                (when (ext:probe-directory directory-form)
                    directory-form))))

    #-(or sbcl cmu lispworks openmcl allegro cmu clisp)
    (error "file-exists-p not implemented"))

(defun pathname-as-file (name)
    (let ((pathname (pathname name)))
        (when (wild-pathname-p pathname)
            (error "Can't reliably convert wild pathnames."))
        (if (directory-p name)
            (let* ((directory (pathname-directory pathname))
                   (name-and-type (pathname (first (last directory)))))
                (make-pathname
                    :directory (butlast directory)
                    :name (pathname-name name-and-type)
                    :type (pathname-type name-and-type)
                    :defaults pathname))
            pathname)))

(defun walk-directory (dirname fn &key directories (test (constantly t)))
    (labels
        ((walk (name)
            (cond ((directory-p name)
                      (when (and directories (funcall test name))
                          (funcall fn name))
                      (dolist (x (list-directory name)) (walk x)))
                  ((funcall test name) (funcall fn name)))))
        (walk (pathname-as-directory dirname))))

;;;
(show-exports *package*)

