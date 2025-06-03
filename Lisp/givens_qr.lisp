; (cdr n) - i
; (car n) - j

(defun givens_proc (R Q n)
    (let*
        (
            (i (cdr n))
            (j (car n))
            (xij (matrix_row_col R i j))
            (xjj (matrix_row_col R j j))
            (cs (compute_givens_rotation xjj xij))
        )
        (progn
            (apply_givens_rotation R cs n (- j 1))
            (apply_givens_rotation Q cs n 0)
            ; (format t "~a: ~a ~a ~%~%" n n n)
        )
    )
)

(defun givens_qr (A)
    (let
        (
            (nomers (create_givens_nomers A))
            (Q (create_identity_matrix (length A)))
            (R (deep_copy A))
        )
        (progn
            (mapcar #'(lambda (n) (givens_proc R Q n)) nomers)
            (cons (transpose_matrix Q) R)
        )
    )
)