
(in-package :cl-user)

(defpackage :common-utils
    (:use :common-lisp)
    (:export :any->string
             :as-keyword
             :slice
             :mklist
             :repeat
             :range
             :string+
             :string*
             :string-join
             :nshuffle-vector
             :return-if
             :getoutput
             :show-exports
             ))

(in-package :common-utils)

(defun any->string (symbol)
    (if (null symbol) ""
        (princ-to-string symbol)))

(defun as-keyword (symbol)
    (intern (symbol-name symbol) :keyword))

(defun mklist (x) (if (listp x) x (list x)))

;;; generate a list of lenth n, filled with item
(defun repeat (item n)
     (loop for i from 1 to n collect item))

;;; generate a list of integer, from low to high
(defun range (low high)
    (if (> low high) nil
        (cons low (range (+ low 1) high))))

;;; get a slice as list[from:to)
(defun slice (lst from to)
    (loop for i from from to (1- to) collect (nth i lst)))

;;; join many strings into one, each parameter should be a string
(defun string+ (&rest items)
    (apply #'concatenate 'string items))

;;; join a list of strings to a string, the first parameter must be a list
(defun string-join (lst &optional sep)
    (if (listp lst)
        (reduce (lambda (a b) (string+ (any->string a)
                                       (any->string sep)
                                       (any->string b)))
                lst)
        lst))

;;; multiple a string by n times
(defun string* (str ntimes)
    (with-output-to-string (stream)
        (dotimes (i ntimes)
            (princ str stream))))

(defun show-exports (package)
    (format t "Package ~A will export:~%" (package-name package))
    (loop for fun being the external-symbols of (find-package package)
        do (format t "~8T~A~%" fun)))

(defun nshuffle-vector (vector)
    (loop for idx downfrom (1- (length vector)) to 1
        for other = (random (1+ idx))
        do (unless (= idx other)
            (rotatef (aref vector idx) (aref vector other))))
    vector)

(defmacro return-if (expr)
    (let ((v (gensym)))
        `(let ((,v ,expr))
             (when ,v (return ,v)))))

#+clisp
(defun getoutput (cmd)
    (with-open-stream (in (ext:make-pipe-input-stream cmd))
        (loop for line = (read-line in nil 'eof)
              while (not (eql line 'eof))
              collect line)))

(show-exports *package*)
