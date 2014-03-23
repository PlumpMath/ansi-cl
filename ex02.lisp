;;; exercise 2-3
(defun fourth-elt (lst)
  (car (cdr (cdr (cdr lst)))))

;;; exercise 2-4
(defun greater (arg1 arg2)
  (if (< arg1 arg2) arg2 arg1))

;;; exercise 2-7
(defun has-list-p (lst)
  (if (null lst)
    nil
    (if (listp (car lst))
      lst
      (has-list-p (cdr lst)))))

;;; exercise 2-8
(defun dots-iter (size)
  (do ((i 0 (+ i 1)))
    ((>= i size) 'done)
    (format t ".")))

(defun dots-recur (size)
  (if (<= size 0)
    nil
    (progn (format t ".")
           (dots-recur (- size 1)))))

(defun count-occurs-iter (lst)
  (do ((i 0 (+ i 1))
       (c 0 (if (equal 'a (nth i lst))
              (+ c 1) c)))
    ((>= i (length lst)) c)))

(defun count-occurs-recur (lst)
  (if (null lst)
    0
    (let ((c (count-occurs-recur (cdr lst))))
      (if (equal 'a (car lst))
        (+ c 1)
        c))))

;;; exercise 2-9
(defun summit-iter (lst)
  (let ((fixed-lst (remove nil lst)))
    (apply #'+ fixed-lst)))

(defun summit-recur (lst)
  (if (null lst)
    0
    (let ((x (car lst)))
      (if (null x)
        (summit-recur (cdr lst))
        (+ x (summit-recur (cdr lst)))))))

