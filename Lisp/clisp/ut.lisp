
(defvar *test-name* nil)

(defmacro combine-results (&body forms)
    (let ( (result (gensym)) )
        `(let ((,result t))
             ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
             ,result)))

(defmacro check (&body forms)
    `(combine-results
         ,@(loop for f in forms collect `(report-result ,f ',f))))

(defun report-result (result form)
    (format t "~:[FAIL~;pass~]  ... ~A => ~A~%" result *test-name* form)
    result)

(defmacro deftest (name parameters &body body)
    `(defun ,name ,parameters
         (let ( (*test-name* ',name) )
             ,@body)))

(deftest test-+ ()
    (check (= (+ 1 2) 3)
           (= (+ 1 2 3) 6)
           (= (+ -1 -3) -5)))

(deftest test-* ()
    (check (= (* 2 2) 4)
           (= (* 3 5) 15)))

(defun test-arithmetic ()
    (combine-results
        (test-+)
        (test-*)))

(test-arithmetic)

