
(defconstant MAXNUM (* 1024 1024 1024))
(defconstant NUM (* 1024 1024)) ; larger will cause stack overflow

(defun gen-random-vector (n)
    (if (= 0 n) ()
        (let ( (a (make-array n :element-type 'integer)))
            (dotimes (i n)
                (setf (aref a i) (random MAXNUM)))
            a)))

(defun qsort1 (lis) 
    (if (null lis) nil
        (let* ( (x (car lis)) 
                (r (cdr lis)) 
                (fn (lambda (a) (< a x))) )
            (append (qsort1 (remove-if-not fn r)) 
                    (list x)
                    (qsort1 (remove-if fn r))))))

(defmacro while (test &rest body)
    `(do ()
        ((not ,test))
        ,@body))

(defun qsort2 (vec l r)
    (declare (type (simple-array integer NUM) a))
    (let ( (i l) 
           (j r) 
           (p (aref vec l)) )
        (while (<= i j)
            (while (< (aref vec i) p) (incf i))
            (while (> (aref vec j) p) (decf j))
            (when (<= i j)
                (rotatef (aref vec i) (aref vec j))
                (incf i)
                (decf j)))
        (if (> j l) (qsort2 vec l j))
        (if (> r i) (qsort2 vec i r))
        vec))

(let ( (lst (time (gen-random-vector NUM))) ) 
    (declare (type (simple-array integer NUM) lst))
    (format t "~%")
    (let ((new-lis (time (qsort2 lst 0 (1- NUM)))) )
;        (format nil "~a~%" new-lis)
    'done))

