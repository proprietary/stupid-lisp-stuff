(defun flatten (l)
  (cond ((null (car l)) (list nil))
        ((atom (car l))
         (cons (car l) (flatten (cdr l))))
        (t (append (flatten (car l)) (flatten (cdr l))))))

(defun distribute (&rest n)
  "Gives the permutations of n lists,
   like the mathematical definition of
   distribution.
   (distribute '(3 8) '(1 2))
   => ((3 1) (8 1) (3 2) (8 2))"
  (if (null (cdr n))
    (car n)
    (mapcon (lambda (x)
              (mapcar (lambda (y)
                        (list y (car x)))
                      (car n))
              (apply #'distribute (cdr n))))))
