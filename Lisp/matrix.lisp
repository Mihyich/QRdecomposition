(defun create_identity_row (nomer_len)
    (and
        (> (cdr nomer_len) 0)
        (cons (cond ((eq (car nomer_len) (cdr nomer_len)) 1)(T 0)) (create_identity_row (cons (car nomer_len) (1- (cdr nomer_len)))))
    )
)

(defun create_list_of_lens (size len)
    (and
        (> size 0)
        (cons (cons size len) (create_list_of_lens (1- size) len))
    )
)

(defun create_identity_matrix (size)
    (mapcar #'create_identity_row (create_list_of_lens size size))
)

(defun deep_copy (object)
    (cond
        ((atom object) object)
        (t (cons (deep_copy (car object)) (deep_copy (cdr object))))
    )
)

(defun matrix_row (m nomer)
    (cond
        ((> nomer 1) (matrix_row (cdr m) (1- nomer)))
        (T (car m))
    )
)

(defun matrix_row_col (m rn cn)
    (nth (- cn 1) (matrix_row m rn))
)

(defun dot_product (a b)
    (reduce #'+ (mapcar #'* a b))
)

(defun transpose_matrix (m)
  (apply #'mapcar #'list m)
)

(defun multiply_matrices (a b)
    (let
        (
            (b-transposed (transpose_matrix b))
        )
        (mapcar
            #'(lambda (row)
              (mapcar
                #'(lambda (col)
                    (dot_product row col)
                )
                b-transposed)
            )
        a)
    )
)

(defun print_matrix (m)
    (dolist (row m)
        (format t "[ ")
        (dolist (col row)
            (format t "~A~1,4T" col)
        )
        (format t "]~%")
    )
)