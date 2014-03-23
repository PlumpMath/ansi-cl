;;; exercise 5-1-a
(defun let-context-1 (y)
  ((lambda (x)
     (cons x x))
   (car y)))

;;; exercise 5-1-b
(defun let-context-2 (x z)
  ((lambda (w)
     ((lambda (y)
        (cons w y))
      (+ w z)))
   (car x)))

;;; exercise 5-2
(defun mystery-cond (x y)
  (cond ((null y) nil)
        ((eql (car y) x) 0)
        (t (let ((z (mystery-cond x (cdr y))))
             (and z (+ z 1))))))

;;; exercise 5-3
(defun square (n)
  (case n
    (1 1)
    (2 4)
    (3 9)
    (4 16)
    (5 25)
    (t (* n n))))

;;; exercise 5-4
(defconstant month
             #(0 31 59 90 120 151 181 212 243 273 304 334 365))

(defconstant yzero 2000)

(defun leap? (y)
  (and (zerop (mod y 4))
       (or (zerop (mod y 400))
           (not (zerop (mod y 100))))))

(defun date->num (d m y)
  (+ (- d 1) (month-num m y) (year-num y)))

(defun month-num (m y)
  (+ (svref month (- m 1))
     (if (and (> m 2) (leap? y)) 1 0)))

(defun year-num (y)
  (let ((d 0))
    (if (>= y yzero)
      (dotimes (i (- y yzero) d)
        (incf d (year-days (+ yzero i))))
      (dotimes (i (- yzero y) (- d))
        (incf d (year-days (+ y i)))))))

(defun year-days (y)
  (if (leap? y)
    366
    365))

;;; exercise 5-5
(defun precedes-iter (x v)
  (labels ((update (pre cur result)
             (if (equal cur x)
               (adjoin pre result)
               result)))
    (do ((i 1 (+ i 1))
          (result nil (update (elt v (- i 1))
                              (elt v i)
                              result)))
      ((>= i (length v))
        (reverse result)))))

(defun precedes-recur (x v)
  (labels ((update (pre cur result)
             (if (equal cur x)
               (adjoin pre result)
               result))
           (traverse (x v result)
             (if (<= (length v) 1)
               result
               (traverse x (subseq v 1) (update (elt v 0) (elt v 1) result)))))
    (reverse (traverse x v nil))))

;;; exercise 5-6
(defun intersperse-iter (obj lst)
  (labels ((update (elt result)
             (cons obj (cons elt result))))
    (do ((dummy lst (cdr dummy))
          (result nil (update (car dummy) result)))
      ((null (cdr dummy))
        (reverse (cons (car dummy) result))))))

(defun intersperse-recur (obj lst)
  (labels ((update (elt result)
             (cons obj (cons elt result)))
           (traverse (lst result)
             (if (null (cdr lst))
               (cons (car lst) result)
               (traverse (cdr lst) (update (car lst) result)))))
    (reverse (traverse lst nil))))

;;; exercise 5-7-a
(defun iff-recur (lst)
  (labels ((check (a b)
             (= (abs (- a b)) 1))
           (traverse (lst)
             (cond ((null (cdr lst)) t)
                   ((check (first lst) (second lst)) (traverse (cdr lst)))
                   (t nil))))
    (if (null (cdr lst))
      nil
      (traverse lst))))

;;; exercise 5-7-b
(defun iff-do (lst)
  (labels ((check (a b)
             (= (abs (- a b)) 1)))
    (if (null (cdr lst))
      nil
      (do ((dummy lst (cdr dummy)))
        ((null (cdr dummy)) t)
        (unless (check (first dummy) (second dummy))
          (return-from iff-do nil))))))

;;; exercise 5-7-c
(defun iff-mapc (lst)
  (if (null lst)
    nil
    (let ((lst1 (subseq lst 0 (- (length lst) 1)))
          (lst2 (subseq lst 1)))
      (progn
        (mapc #'(lambda (a b)
                  (unless (= (abs (- a b)) 1)
                    (return-from iff-mapc)))
              lst1
              lst2)
        t))))

;;; exercise 5-8
(defun minmax-vector (v)
  (labels ((traverse (min max i)
             (if (>= i (length v))
               (values min max t)
               (let ((elt (svref v i)))
                 (when (< elt min) (setf min elt))
                 (when (> elt max) (setf max elt))
                 (traverse min max (+ i 1))))))
    (traverse (svref v 0) (svref v 0) 0)))

;;; exercise 5-9
