(defun hypot (a b)
    (sqrt (+ (* a a) (* b b)))
)

(defun compute_givens_rotation (a b)
    (let
        ((r (hypot a b)))
        (cons (/ a r) (/ (- b) r))
    )
)

(defun local_rotation (c s x y)
    (cons (- (* c x) (* s y)) (+ (* s x) (* c y)))
)

(defun apply_givens_rotation (m cs ii scol)
    (let*
        (
            (c (car cs))
            (s (cdr cs))
            (i1 (car ii))
            (i2 (cdr ii))
            (row1 (matrix_row m i1))
            (row2 (matrix_row m i2))
            (rots (mapcar #'(lambda (x y) (local_rotation c s x y)) row1 row2))
            (rxs (mapcar #'(lambda (r) (car r)) rots))
            (rys (mapcar #'(lambda (r) (cdr r)) rots))
        )
        (progn
            (replace row1 rxs :start1 scol :start2 scol)
            (replace row2 rys :start1 scol :start2 scol)
        )
    )
)