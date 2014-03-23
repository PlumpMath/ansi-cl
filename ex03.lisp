;;; run-length compress algorithm
(defun compress (x)
  (if (consp x)
    (compr (car x) 1 (cdr x))
    x))

(defun compr (elt n lst)
  (if (null lst)
    (list (n-elts elt n))
    (let ((next (car lst)))
      (if (eql next elt)
        (compr elt (+ n 1) (cdr lst))
        (cons (n-elts elt n)
              (compr next 1 (cdr lst)))))))

(defun n-elts (elt n)
  (if (> n 1)
    (list n elt)
    elt))

(defun uncompress (lst)
  (if (null lst)
    nil
    (let ((elt (car lst))
          (rest (uncompress (cdr lst))))
      (if (consp elt)
        (append (apply #'list-of elt)
                rest)
        (cons elt rest)))))

(defun list-of (n elt)
  (if (zerop n)
    nil
    (cons elt (list-of (- n 1) elt))))

(defun yeono-compress (lst)
  (labels ((n-elts (elt n)
             (if > n 1)
             (list n elt)
             elt)
           (compr (elt n lst)
             (let ((next (car lst)))
               (if (null lst)
                 (n-elts elt n)
                 (if (eql elt next)
                   (compr (elt (+ n 1) (cdr lst)))
                   (cons (n-elts elt n) (compr (next 1 (cdr lst))))))))))
  (if (consp lst)
    (compr (car lst) 1 (cdr lst))
    lst))

(defun yeono-uncompress (lst)
  (labels ((n-elts (elt)
             (if (consp elt)
               (do ((i 0 (+ i 1))
                    (c nil (cons (car (cdr elt)) c)))
                 ((>= i (car elt))
                  c))
               (cons elt nil)))
           (uncompr (lst)
             (if (null lst)
               nil
               (append (n-elts (car lst)) (uncompr (cdr lst))))))
    (if (consp lst)
      (uncompr lst)
      lst)))

;;; breadth-first search algorithm
(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

(defun bfs (end queue net)
  (if (null queue)
    nil
    (let ((path (car queue)))
      (let ((node (car path)))
        (if (eql node end)
          (reverse path)
          (bfs end
               (append (cdr queue)
                       (new-paths path node net))
               net))))))

(defun new-paths (path node net)
  (mapcar #'(lambda (n)
              (cons n path))
          (cdr (assoc node net))))

