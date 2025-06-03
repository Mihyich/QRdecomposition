(load "Lisp/matrix.lisp")
(load "Lisp/givens_rot.lisp")
(load "Lisp/givens_nomers.lisp")
(load "Lisp/givens_qr.lisp")


; (defvar A (create_identity_matrix 3))
(defvar A 
    `(
        (1 0)
        (1 1)
    )
)

; (format t "~a~%~%" (deep_copy A))

; (format t "~a~%~%" (compute_givens_rotation 3 4))

; (format t "~a~%~%" (apply_givens_rotation A (compute_givens_rotation 3 4) (cons 1 2) 0))

; (format t "~a" (create_identity_row (cons 2 5)))

; (format t "~a" (cdr (cons 2 5)))

(defvar QR (givens_qr A))
(defvar Q (car QR))
(defvar R (cdr QR))

(format t "A:~%")
(print_matrix A)

(format t "~%Q:~%")
(print_matrix Q)

(format t "~%R:~%")
(print_matrix R)

(format t "~%QR:~%")
(print_matrix (multiply_matrices Q R))