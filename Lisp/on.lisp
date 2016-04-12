
(defun make-dbms (db)
    (list
        #'(lambda (key)
            (cdr (assoc key db)))
        #'(lambda (key val)
            (push (cons key val) db)
            key)
        #'(lambda (key)
            (setf db (delete key db :key #'car))
            key)))

(defun lookup (key db) (funcall (car db) key))

(proclaim '(inline last1 single append1 conc1 mklist))

(defun last1(lst) (car (last lst)))

(defun single(lst) (and (consp lst) (not (cdr lst))))

(defun append1 (lst obj) (append lst (list obj)))

(defun conc1 (lst obj) (nconc lst (list obj)))

(defun mklist (obj) (if (listp obj) obj (list obj)))

(defun longer (x y)
    (labels ( (compare (x y)
                 (and (consp x) (or (null y)
                                    (compare (cdr x) (cdr y))))) )
        (if (and (listp x) (listp y))
            (compare x y)
            (> (length x) (length y)))))

(defun filter (fn lst)
    (let ( (acc nil) )
        (dolist (x lst)
            (let ( (val (funcall fn x)) )
                (if val (push val acc))))
        (nreverse acc)))

(defun group (source n)
    (if (zerop n) (error "zero length"))
    (labels ( (rec (source acc)
                  (let ( (rest (nthcdr n source)) )
                      (if (consp rest)
                          (rec rest (cons (subseq source 0 n) acc))
                          (nreverse (cons source acc))))))
        (if source (rec source nil) nil)))

(defun flattern (x)
    (labels ( (rec (x acc)
                  (cond ((null x) acc)
                        ((atom x) (cons x acc))
                        (t (rec (car x) (rec (cdr x) acc))))) )
        (rec x nil)))

(defun prune (test tree)
    (labels ( (rec (tree acc)
                  (cond ((null tree) (nreverse acc))
                        ((consp (car tree))
                             (rec (cdr tree)
                                  (cons (rec (car tree) nil) acc)))
                        (t (rec (cdr tree)
                                (if (funcall test (car tree))
                                    acc
                                    (cons (car tree) acc)))))) )
        (rec tree nil)))

(defun find2 (fn lst)
    (if (null lst)
        nil
        (let ( (val (funcall fn (car lst))) )
            (if val (values (car lst) val)
                    (find2 fn (cdr lst))))))

(defun before (x y lst &key (test #'eql))
    (and lst
         (let ( (first (car lst)) )
             (cond ((funcall test y first) nil)
                   ((funcall test x first) lst)
                   (t (before x y (cdr lst) :test test))))))

(defun after (x y lst &key (test #'eql))
    (let ( (rest (before y x lst :test test)) )
        (and rest (member x rest :test test))))

(defun duplicate (obj lst &key (test #'eql))
    (memeber obj (cdr (member obj lst :test test)) :test test))

(defun split-if (fn lst)
    (let ( (acc nil) )
        (do ((src lst (cdr src)))
            ((or (null src) (funcall fn (car src)))
                (values (nreverse acc) src))
            (push (car src) acc))))

(defun mapa-b (fn a b &optional (step 1))
    (do ((i a (+ i step)) (result nil))
        ((> i b) (nreverse result))
        (push (funcall fn i) result)))

(defun map0-n (fn n) (mapa-b fn 0 n))
(defun map1-n (fn n) (mapa-b fn 1 n))

(defun map-> (fn start test-fn succ-fn)
    (do ((i start (funcall succ-fn i)) (result nil))
        ((funcall test-fn i) (nreverse result))
        (push (funcall fn i) result)))

(defun mappend (fn &rest lsts)
    (apply #'append (apply #'mapcar fn lsts)))

(defun mapcars (fn &rest lsts)
    (let ((result nil))
        (dolist (lst lsts)
            (dolist (obj lst)
                (push (funcall fn obj) result)))
        (nreverse result)))

(defun rmapcar (fn &rest args)
    (if (some #'atom args)
        (apply fn args)
        (apply #'mapcar #'(lambda (&rest args)
                              (apply #'rmapcar fn args)) args)))

(defun readlist (&rest args)
    (values (read-from-string
                (concatenate 'string "("
                                     (apply #'read-line args)
                                     ")"))))

(defun prompt (&rest args)
    (apply #'format *query-io* args)
    (read *query-io*))

(defun break-loop (fn quit &rest args)
    (format *query-io* "Entering break-loop. ~%")
    (loop (let ((in (apply #'prompt args)))
        (if (funcall quit in)
            (return)
            (format *query-io* "~a~%" (funcall fn in)))))))

(defun mkstr (&rest args)
    (with-output-to-string(s)
        (dolist (a args)
            (princ a s))))

(defun symb (&rest args)
    (values (intern (apply #'mkstr args))))

(defun reread (&rest args)
    (values (read-from-string (apply #'mkstr args))))

(defun explode (sym)
    (map 'list #'(lambda (c) (intern (make-string 1 :initial-element c)))
        (symbol-name sym)))

(defvar *!equivs* (make-hash-table))

(defun ! (fn)
    (or (gethash fn *!equivs*) fn))

(defun def! (fn fn!)
    (setf (gethash fn *!equivs*) fn!))

(defun memoize (fn)
    (let ((cache (make-hash-table :test #'equal)))
        #'(lambda (&rest args)
              (multiple-value-bind (val win) (gethash args cache)
                  (if win
                      val
                      (setf (gethash args cache) (apply fn args)))))))

(defun compose (&rest fns)
    (if fns
        (let ((fn1 (car (last fns)))
              (fns (butlast fns)))
            #'(lambda (&rest args)
                (reduce #'funcall fns :from-end t :initial-value (apply fn1 args))))
        #'identity))

(defun fif (if then &optional else)
    #'(lambda (x)
        (if (funcall if x)
            (funcall then x)
            (if else (funcall else x)))))

(defun fint (fn &rest fns)
    (if (null fns)
        fn
        (let ((chain (apply #'fint fns)))
            #'(lambda (x)
                (and (funcall fn x) (funcall chain x))))))
(defun fun (fn &rest fns)
    (if (null fns)
        fn
        (let ((chain (apply #'fun fns)))
            #'(lambda (x)
                (or (funcall fn x) (funcall chain x))))))

(defun lrec (rec &optional base)
    (labels (
        (self (lst)
            (if (null lst)
                (if (functionp base) (funcall base) base)
                (funcall rec (car lst) #'(lambda () (self (cdr lst)))))))
        #'self))

(defmacro our-dolist ((var list &optional result) &body body)
	`(progn (mapc #'(lambda (,var) ,@body) ,list)
		(let ((,var nil)) ,result)))

(defmacro when-bind ((var expr) &body body)
	`(let ((,var ,expr))
		(when ,var ,@body)))

(defmacro our-expander (name)
	`(get ,name 'expender))

(defmacro our-defmacro (name params &body body)
	(let ((g (gensym)))
        `(progn 
            (setf (our-expander ',name)
                  #'(lambda (,g)
                      (block 
                          ,name
                          (destructing-bind ,params (cdr ,g) ,@body))))
            ',name)))

(defun our-macroexpand-1 (expr)
    (if (and (consp expr) (our-expander (car expr)))
        (funcall (our-expander (car expr)) expr) expr))

(defmacro our-do (bindforms (test &rest result) &body body)
    (let ((label (gensym)))
        `(progn
            (make-initforms bindforms)
            ,label (if ,test (return (progn ,@result)))
                   ,@body
                   (psetq ,@(make-stepforms bindforms))
                   (go ,label))))

(defun make-initforms (bindforms)
    (mapcar #'(lambda (b)
                (if (consp b) (list (car b) (cadr b)) (list b nil)))
            bindforms))

(defun make-stepforms (bindforms)
    (mapcan #'(lambda (b)
                (if (and (consp b) (third b))
                    (list (car b) (third b))
                    nil))
            bindforms))

