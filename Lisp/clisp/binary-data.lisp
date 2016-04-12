
(in-package :cl-user)

(defpackage :binary-data
    (:use :common-lisp
          :common-utils
          :macro-utilities)
    (:export :define-binary-class
             :define-tagged-binary-class
             :define-binary-type
             :read-value
             :write-value
             :*in-progress-objects*
             :parent-of-type
             :current-binary-object
             :+null+))

(in-package :binary-data)

(defconstant +null+ 0)

(defun slot->defclass-slot (spec)
    (let ((name (first spec)))
        `(,name :initarg ,(as-keyword name) :accessor ,name)))

(defun normalize-slot-spec (spec)
    (list (first spec) (mklist (second spec))))

(defun slot->read-value (spec stream)
    (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
        `(setf ,name (read-value ',type ,stream ,@args))))

(defun slot->write-value (spec stream)
    (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
        `(write-value ',type ,stream ,name ,@args)))

(defgeneric read-value (type stream &key)
    (:documentation "Read a value of the given type from the stream."))

(defgeneric write-value (type stream value &key)
    (:documentation "Write a value of the given type to the stream."))

(defgeneric read-object (object stream)
    (:method-combination progn :most-specific-last)
    (:documentation "Fill in the slots of object from stream."))

(defgeneric write-object (object stream)
    (:method-combination progn :most-specific-last)
    (:documentation "Write out the slots of object to stream."))

(defmethod read-value ( (type symbol) stream &key )
    (let ((object (make-instance type)))
        (read-object object stream)
        object))

(defmethod write-value ( (type symbol) stream value &key )
    (assert (typep value type))
    (write-object value stream))

(defun direct-slots (name)
    (copy-list (get name 'slots)))

(defun inherited-slots (name)
    (loop for super in (get name 'superclasses)
          nconc (direct-slots super)
          nconc (inherited-slots super)))

(defun all-slots (name)
    (nconc (direct-slots name) (inherited-slots name)))

(defun new-class-all-slots (slots superclasses)
    (nconc (mapcan #'all-slots superclasses) (mapcar #'first slots)))

(defmacro define-generic-binary-class (name (&rest superclasses)
                                       slots read-method)
    (with-gensyms (objectvar streamvar)
        `(progn
            (eval-when (:compile-toplevel :load-toplevel :execute)
                (setf (get ',name 'slots) ',(mapcar #'first slots))
                (setf (get ',name 'superclasses) ',superclasses))
            (defclass ,name ,superclasses
                ,(mapcar #'slot->defclass-slot slots))

            ,read-method

            (defmethod write-object progn ((,objectvar ,name) ,streamvar)
              (declare (ignorable ,streamvar))
                (with-slots ,(new-class-all-slots slots superclasses)
                            ,objectvar
                    ,@(mapcar (lambda (x) (slot->write-value x streamvar))
                              slots))))))

(defmacro define-binary-class (name (&rest superclasses) slots)
    (with-gensyms (objectvar streamvar)
        `(define-generic-binary-class ,name ,superclasses ,slots
            (defmethod read-object progn ((,objectvar ,name) ,streamvar)
                (declare (ignorable ,streamvar))
                (with-slots ,(new-class-all-slots slots superclasses) ,objectvar
                    ,@(mapcar (lambda (x) (slot->read-value x streamvar))
                              slots))))))

(defmacro define-tagged-binary-class (name (&rest superclasses)
                                      slots &rest options)
    (with-gensyms (typevar objectvar streamvar)
        `(define-generic-binary-class ,name ,superclasses ,slots
            (defmethod read-value ((,typevar (eql ',name)) ,streamvar &key)
                (let* ,(mapcar (lambda (x) (slot->binding x streamvar))
                               slots)
                    (let ((,objectvar
                           (make-instance
                               ,@(or (cdr (assoc :dispatch options))
                                     (error "Must supply :dispatch form."))
                               ,@(mapcan #'slot->keyword-arg slots))))
                        (read-object ,objectvar ,streamvar)
                        ,objectvar))))))

(defun slot->binding (spec stream)
    (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
        `(,name (read-value ',type ,stream ,@args))))

(defun slot->keyword-arg (spec)
    (let ((name (first spec)))
        `(,(as-keyword name) ,name)))

(defmacro define-binary-type (name (&rest args) &body spec)
    (ecase (length spec)
        (1
            (with-gensyms (type stream value)
                (destructuring-bind (derived-form &rest derived-args)
                                    (mklist (first spec))
                    `(progn
                        (defmethod read-value ((,type (eql ',name))
                                               ,stream &key ,@args)
                            (read-value ',derived-form ,stream ,@derived-args))
                        (defmethod write-value ((,type (eql ',name))
                                                ,stream ,value &key ,@args)
                            (write-value ',derived-form ,stream ,value
                                          ,@derived-args))))))
        (2
            (unless (and (assoc :reader spec) (assoc :writer spec))
                (error "Must provide the :reader and :writer method"))
            (with-gensyms (type)
                `(progn
                    ,(destructuring-bind ((in) &body body)
                                         (rest (assoc :reader spec))
                        `(defmethod read-value ((,type (eql ',name))
                                                ,in &key ,@args)
                            ,@body))
                    ,(destructuring-bind ((out value) &body body)
                                         (rest (assoc :writer spec))
                        `(defmethod write-value ((,type (eql ',name))
                                                ,out ,value &key ,@args)
                            ,@body)))))))

(defvar *in-progress-objects* nil)

(defmethod read-object :around (object stream)
    (declare (ignorable stream))
    (let ((*in-progress-objects* (cons object *in-progress-objects*)))
        (call-next-method)))

(defmethod write-object :around (object stream)
    (declare (ignorable stream))
    (let ((*in-progress-objects* (cons object *in-progress-objects*)))
        (call-next-method)))

(defun current-binary-object ()
    (first *in-progress-objects*))

(defun parent-of-type (type)
    (find-if (lambda (x) (typep x type)) *in-progress-objects*))

(show-exports *package*)

;;; test
;(macroexpand-1 '(define-binary-class id3-tag ()
;    ((identifier      (iso-8859-1-string :length 3))
;     (major-version   u1)
;     (revision        u1)
;     (flags           u1)
;     (size            id3-tag-size)
;     (frames          (id3-frames :tag-size size)))))

;(defvar reader '(:reader (in)
;        (let ((string (make-string length)))
;            (dotimes (i length)
;                (setf (char string i) (code-char (read-byte in))))
;            string)))
;(defvar writer '(:writer (out string)
;        (dotimes (i length)
;            (write-byte (char-code (char string i)) out))))
;
;(macroexpand-1 (list 'define-binary-type
;                     'iso-8859-1-string
;                     '(length)
;                     reader
;                     writer))
