;;; exercise 4-1
(defun print-2a (arr)
  (let* ((len (car (array-dimensions arr)))
         (last (- len 1)))
    (do ((x 0 (if (< x last)
                (+ x 1)
                0))
         (y 0 (if (< x last)
                y
                (progn (format t "~%")
                       (+ y 1)))))
      ((> y last)
       arr)
      (format t "~A " (aref arr y x)))))

(defun rotate-2a (arr)
  (let* ((len (car (array-dimensions arr)))
         (last (- len 1))
         (result (make-array `(,len ,len))))
    (do ((x 0 (if (< x last)
                (+ x 1)
                0))
         (y 0 (if (< x last)
                y
                (+ y 1))))
      ((> y last)
       result)
      (setf (aref result y x) (aref arr (- last x) y)))))

;;; exercise 4-2-a
(defun copy-list-on-reduce (lst)
  (reduce #'(lambda (a b) (cons a b))
          lst
          :initial-value nil
          :from-end t))

;;; exercise 4-2-b
(defun reverse-on-reduce (lst)
  (reduce #'(lambda (a b) (cons b a))
          lst
          :initial-value nil))

;;; exercise 4-3
(defstruct 3c-tree
  val
  c0
  c1
  c2)

(setf 3c-tree-test
      #S(3C-TREE :VAL apple
         :C0 #S(3C-TREE :VAL diamond
                :C0 #S(3C-TREE :VAL alogrithm
                       :C0 nil
                       :C1 nil
                       :C2 nil)
                :C1 #S(3C-TREE :VAL ultramarine
                       :C0 #S(3C-TREE :VAL orange
                              :C0 #S(3C-TREE :VAL clojure
                                     :C0 nil
                                     :C1 nil
                                     :C2 nil)
                              :C1 nil
                              :C2 nil)
                       :C1 nil
                       :C2 nil)
                :C2 #S(3C-TREE :VAL unix
                       :C0 nil
                       :C1 nil
                       :C2 nil)
                )
         :C1 nil
         :C2 nil))

(defun copy-3c-tree (src)
  (make-3c-tree :val (3c-tree-val src)
                :c0 (if (null (3c-tree-c0 src))
                      nil
                      (copy-3c-tree (3c-tree-c0 src)))
                :c1 (if (null (3c-tree-c1 src))
                      nil
                      (copy-3c-tree (3c-tree-c1 src)))
                :c2 (if (null (3c-tree-c2 src))
                      nil
                      (copy-3c-tree (3c-tree-c2 src)))))

(defun in-3c-tree (obj 3ct)
  (if (3c-tree-p 3ct)
    (if (eql obj (3c-tree-val 3ct))
      obj
      (some #'(lambda (x) (in-3c-tree obj x))
            `(,(3c-tree-c0 3ct) ,(3c-tree-c1 3ct) ,(3c-tree-c2 3ct))))
    nil))

;;; exercise 4-4




;;; exercise 4-5



;;; exercise 4-6-a
(defun assoc-to-hash (assoc)
  (labels ((to-hash-recur (result rest)
             (if (null rest)
               result
               (let ((key (car (car rest)))
                     (value (cdr (car rest))))
                 (progn (setf (gethash key result) value)
                        (to-hash-recur result (cdr rest)))))))
    (to-hash-recur (make-hash-table) (reverse assoc))))

;;; exercise 4-6-b
(defun hash-to-assoc (ht)
  (let ((assoc nil))
    (progn
      (maphash #'(lambda (k v) (setf assoc (cons (cons k v) assoc))) ht)
      (reverse assoc))))
