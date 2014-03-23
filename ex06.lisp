;;; exercise 6-1
(defun tokens (str test start)
  (let ((p1 (position-if test str :start start)))
    (if p1
      (let ((p2 (position-if #'(lambda (c)
                                 (not (funcall test c)))
                             str :start p1)))
        (cons (subseq str p1 p2)
              (if p2
                (tokens str test p2)
                nil)))
      nil)))

(defun constituent (c)
  (and (graphic-char-p c)
       (not (char= c #\ ))))

(defun tokens-with-keys (str &key (test #'constituent) (start 0))
  (let ((p1 (position-if test str :start start)))
    (if p1
      (let ((p2 (position-if #'(lambda (c)
                                 (not (funcall test c)))
                             str :start p1)))
        (cons (subseq str p1 p2)
              (if p2
                (tokens-with-keys str :test test :start p2)
                nil)))
      nil)))

;;; exercise 6-2


;;; exercise 6-3
(defun count-arg (&rest args)
  (length args))

;;; exercise 6-4
(defun most (fn lst)
  (if (null lst)
    (values nil nil)
    (let* ((wins (car lst))
           (max (funcall fn wins)))
      (dolist (obj (cdr lst))
        (let ((score (funcall fn obj)))
          (when (> score max)
            (setf wins obj
                  max score))))
      (values wins max))))

(defun two-most (fn lst)
  (cond ((null lst)
         (values nil nil nil nil))
        ((null (cadr lst))
         (values (car lst) (funcall fn lst) nil nil))
        (t
         (let* ((1st (car lst))
                (1st-score (funcall fn 1st))
                (2nd nil)
                (2nd-score nil))
           (dolist (obj (cdr lst))
             (let ((obj-score (funcall fn obj)))
               (cond ((> obj-score 1st-score)
                      (setf 2nd 1st
                            2nd-score 1st-score
                            1st obj
                            1st-score obj-score))
                     ((> obj-score 2nd-score)
                      (setf 2nd obj
                            2nd-score obj-score)))))
           (values 1st 2nd)))))

;;; exercise 6-5
(defun my-remove-if (fn lst)
  (let ((acc nil))
    (dolist (obj lst)
      (unless (funcall fn obj)
        (setf result (push obj acc))))
    (nreverse acc)))

;;; exercise 6-6
(let ((max nil))
  (defun max-so-far (n)
    (progn
      (when (or (null max)
                (> n max))
        (setf max n))
      max)))

;;; exercise 6-7
(let ((prev nil))
  (defun greater-than-prev-p (n)
    (let ((old prev))
      (progn
        (setf prev n)
        (if (null old)
          nil
          (< old n))))))

;;; exercise 6-8
(defun expansive (n)
  (progn
    (format t "A lot of resource has been comsumed!")
    (* n n n n)))

(let ((memo nil))
  (defun frugal (n)
    (let ((pre-computed (assoc n memo)))
      (if pre-computed
        (cdr pre-computed)
        (let ((computed (expansive n)))
          (progn
            (push (cons n computed) memo)
            computed))))))

;;; exercise 6-9
