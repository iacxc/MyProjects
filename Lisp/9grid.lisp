
(load "common-utils")
(use-package :common-utils)

(defconstant NUM (* 9 9))

(defun samerow (grid idx)
; get all the values in the same row with idx
    (let ((row_num (floor (/ idx 9))))
        (slice grid (* row_num 9) (* (1+ row_num) 9))))

(defun samecol (grid idx)
; get all the values in the same col with idx
    (let ( (col_num (rem idx 9)) )
        (loop for row from 0 to 8
              collect (elt grid (+ (* row 9) col_num)))))

(defun samesquare (grid idx)
; " get all the values in the same square with idx
;    grid)
    (let ((blk_row (floor (/ idx 9 3)))
          (blk_col (floor (/ (rem idx 9) 3))))
       (let ((start (+ (* blk_row 9 3) (* blk_col 3))))
           (append (slice grid start (+ start 3))
                   (slice grid (+ start 9) (+ start 9 3))
                   (slice grid (+ start 18) (+ start 18 3))))))

(defun formatrow (grid rownum)
    (let* ( (start (* rownum 9))
            (part1 (string-join (slice grid start (+ start 3)) "  "))
            (part2 (string-join (slice grid (+ start 3) (+ start 6)) "  "))
            (part3 (string-join (slice grid (+ start 6) (+ start 9)) "  ")) )
        (string-join (list (string+ " " part1) part2 part3) " | ")))

(defun formatgrid (grid)
    (labels ( (_formatrow (i) (formatrow grid i)) )
        (string+
            (string-join (mapcar #'_formatrow '(0 1 2)) #\Newline)
            (string #\Newline)
            (string-join (repeat (string* "-" 9) 3) "+")
            (string #\Newline)
            (string-join (mapcar #'_formatrow '(3 4 5)) #\Newline)
            (string #\Newline)
            (string-join (repeat (string* "-" 9) 3) "+")
            (string #\Newline)
            (string-join (mapcar #'_formatrow '(6 7 8)) #\Newline))))

(defun solve9grid (grid &optional (start 0))
;    (format t "~a: ~a~%" start grid)
    (if (= start NUM)
        grid
        (if (zerop (elt grid start))
            (let ((mask (append (samerow grid start)
                                (samecol grid start)
                                (samesquare grid start))))
                (dotimes (val 9)
                    (when (not (find (1+ val) mask))
                        (return-if
                            (solve9grid (append (slice grid 0 start)
                                                (list (1+ val))
                                                (slice grid (1+ start) NUM))
                                        (1+ start))))))
            (solve9grid grid (1+ start)))))

;;;--------------------------------
(let ((grid '(8 0 0 0 0 6 1 0 0
              0 0 0 5 0 0 0 0 4
              0 0 4 2 8 3 5 0 9
              0 0 6 0 0 0 0 0 3
              0 2 9 0 0 1 0 0 0
              5 0 3 0 0 0 0 4 0
              0 0 0 8 5 0 0 0 0
              0 0 0 0 0 2 0 6 0
              0 0 0 3 0 4 0 2 0)))
    (let ((solved (time (solve9grid grid))))
        (princ (formatgrid solved))))

;(defparameter *grid* '(8 0 0 0 0 6 1 0 0
;              0 0 0 5 0 0 0 0 4
;              0 0 4 2 8 3 5 0 9
;              0 0 6 0 0 0 0 0 3
;              0 2 9 0 0 1 0 0 0
;              5 0 3 0 0 0 0 4 0
;              0 0 0 8 5 0 0 0 0
;              0 0 0 0 0 2 0 6 0
;              0 0 0 3 0 4 0 2 0))
;
;(princ (formatgrid *grid*))
;(format t "~&~a ~a" (nth 13 *grid*) (samerow *grid* 13))
;(format t "~&~a ~a" (nth 13 *grid*) (samecol *grid* 13))
;(format t "~&~a ~a" (nth 13 *grid*) (samesquare *grid* 13))

;(princ (formatgrid (solve9grid *grid*)))
(quit)
