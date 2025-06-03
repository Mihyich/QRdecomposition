(defun expand_indeces (col_nomer rows)
    (and
        (> rows col_nomer)
        (cons (cons col_nomer rows) (expand_indeces col_nomer (1- rows)))
    )
)

(defun create_list_of_bot (size)
    (and
        (> size 0)
        (cons size (create_list_of_bot (1- size)))
    )
)

(defun create_givens_nomers (m)
    (let
        (
            (rows (length m))
        )
        (reduce #'append (mapcar #'(lambda (x) (expand_indeces x rows)) (reverse (create_list_of_bot (length (car m))))))
    )
)