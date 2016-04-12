
(set-macro-character #\$
                     #'(lambda (stream char)
                         (declare (ignore char))
                         (round (* 100 (read stream)))))

(defvar *checks* (make-array 100 :adjustable t :fill-pointer 0)
    "A vector of checks.")

(defconstant +first-check-number+ 100
    "The number of first check.")

(defvar *next-check-number* +first-check-number+
    "The number of the next check.")

(defvar *payees* (make-hash-table :test #'equal)
    "Payees with checks paid to each.")

(defstruct (check
             (:print-function
                 (lambda (check stream depth)
                     (declare (ignore depth))
                     (format stream "#S(Check Number ~S Date ~S Amount $~,2,-2F Payee ~S Memo ~S)"
                         (check-number check)
                         (check-date check)
                         (check-amount check)
                         (check-payee check)
                         (check-memo check)))))
    number date amount payee memo)

(defun current-date-string()
    "Returns current date as a string."
    (multiple-value-bind (sec min hour day mon year dow dsp-p tz)
                         (get-decoded-time)
        (declare (ignore sec min hour dow dst-p tz))
        (format nil "~A-~A-~A" year mon day)))

(defun write-check (amount payee memo)
    "Writes the next check in sequence."
    (let ((new-check (make-check
                      :number *next-check-number*
                      :date (current-date-string)
                      :amount amount
                      :payee payee
                      :memo memo)))
        (incf *next-check-number*)
        (vector-push-extend new-check *checks*)
        (push new-check (gethash payee *payees*))
        new-check))

(defun get-check (number)
    "Returns a check give its number, or NIL if no such check."
    (when (and (<= +first-check-number+ number) (< number *next-check-number*))
        (aref *checks* (- number +first-check-number+))))

(defun void-check (number)
    "Voids a check and returns T. Returns NIL if no such check. "
    (let ((check (get-check number)))
        (when check
            (setf (gethash (check-payee check) *payees*)
                  (delete check (gethash (check-payee check) *payees*)))
            (setf (aref *checks* (- number +first-check-number+)) nil)
            t)))

(defun list-checks (payee)
    "Lists all of the checks written to payee."
    (gethash payee *payees*))

(defun list-all-checks ()
    "Lists all checks written."
    (coerce *checks* 'list))

(defun sum-checks ()
    (let ((sum 0))
        (map nil #'(lambda (check)
                      (when check (incf sum (check-amount check))))
            *checks*)
        sum))

(defun list-payees ()
    "Lists all payees."
    (let ((payees ()))
        (maphash #'(lambda (key value)
                       (declare (ignore value))
                       (push key payees))
                 *payees*)
        payees))

(defmacro def-i/o (writer-name reader-name (&rest vars))
    (let ((file-name (gensym)) (var (gensym)) (stream (gensym)))
        `(progn
            (defun ,writer-name (,file-name)
                (with-open-file (,stream ,file-name
                                     :direction :output :if-exists :supersede)
                    (dolist (,var (list ,@vars))
                        (declare (special ,@vars))
                        (print ,var ,stream))))
            (defun ,reader-name, (,file-name)
                (with-open-file (,stream ,file-name
                                    :direction :input :if-does-not-exist :error)
                    (dolist (,var ',vars)
                        (set ,var (read ,stream)))))
            t)))

(def-i/o save-checks load-checks (*checks* *next-check-number* *payees*))

