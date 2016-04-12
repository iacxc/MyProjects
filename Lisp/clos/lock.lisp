
(defclass lock ()
    ((name :initarg :name :reader lock-name))
    (:documentation "The foundation of all locks."))

(defclass null-lock (lock)
    ()
    (:documentation "A lock that is always free."))
	
(defclass simple-lock (lock)
    ((owner :initform nil :accessor lock-owner))
    (:documentation "A lock that is either free or busy."))
	
(defun make-null-lock (name)
    (make-instance 'null-lock :name name))
	
(defun make-simple-lock (name)
    (make-instance 'simple-lock :name name))
	
(defgeneric seize (lock)
    (:documentation
"Seizes the lock.
Returns the lock when operation succeeds.
Some locks simply wait until they can succeed, while other locks
return nil if they fail."))

(defgeneric release (lock &optional failure-mode)
    (:documentation
"Releases the lock if it is currently owned by this process.
Returns T if the operation succeeds.
If unsuccessful and failure-mode is :no-error, returns nil,
if unsuccessful and failure-mode is :error, signals an error.
The default for failure-mode is :no-error."))

(defmethod seize ((l null-lock)) l) ; return lock, no waiting
(defmethod release ((l null-lock) &optional failure-mode)
    (declare (ignorable failure-mode))    ;never fails for null locks
    t)
    
(defmacro setf-if (place old-place new-place)
    `(without-process-preemption
        (cond ((eql ,place ,old-place) (setf ,place ,new-place) t)
              (t nil))))
              
(defmethod check-for-mylock ((l simple-lock) process)
    (when (eql (lock-owner l) process)
        (error "Can't seize ~a because you already own it." l)))

(defmethod seize ((l simple-lock))
    (check-for-mylock l *current-process*)
    (do ()
        ((setf-if (lock-owner l) nil *current-process*))
        (process-wait "Seizing lock"
                      #'(lambda () (null (lock-owner l)))))
    l)

(defmethod release ((l simple-lock) &optional (failure-mode :no-error))
    (or (setf-if (lock-owner l) *current-process* nil)
        (ecase failure-mode
            (:no-error nil)
            (:error (error "~a is not owned by this process" l)))))
            
(defmethod print-object ((l lock) stream)
    (format stream "#<~s \"~a\" 0x~x>" 
        (type-of l) 
        (if (slot-boundp l 'name)
            (lock-name l)            
            "(no name)")
        (sys::address-of l))
    l)
   
(defmethod describe-object ((l lock) stream)
    (format stream "a lock of type ~s named \"~a\"."
        (type-of l) 
        (if (slot-boundp l 'name) (lock-name l) "(no name)"))
    (values))

(defmethod describe-object :after ((l simple-lock) stream)
    (let ((owner (lock-owner l)))
        (format stream (if owner "~&It is now owned by process ~a."
                            "~&It is now free.") owner)))
                            
(setf sl (make-simple-lock "Simple lock 1"))
(setf nl (make-null-lock "Null lock 1"))                            
                            
