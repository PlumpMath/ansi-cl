;;; exercise 7-1
(defun read-lines-from-file (filename)
  (let* ((path (make-pathname :name filename))
         (ifs (open path :direction :input))
         (acc nil))
    (do ((line (read-line ifs nil :end-of-file)
               (read-line ifs nil :end-of-file)))
      ((eql line :end-of-file)
       (nreverse acc))
      (push line acc))))

;;; exercise 7-2
(defun read-exps-from-file (filename)
  (let* ((path (make-pathname :name filename))
         (ifs (open path :direction :input))
         (acc nil))
    (do ((exp (read ifs nil :__end-of-file__yeono)
              (read ifs nil :__end-of-file__yeono)))
      ((eql exp :__end-of-file__yeono)
       (nreverse acc))
      (push exp acc))))

;;; exercise 7-3
(defun copy-without-comment (src dst)
  (let* ((src-path (make-pathname :name src))
         (dst-path (make-pathname :name dst))
         (src-fs (open src-path :direction :input))
         (dst-fs (open dst-path :direction :output :if-exists :supersede)))
    (do ((line (read-line src-fs nil :end-of-line)
               (read-line src-fs nil :end-of-line)))
      ((eql line :end-of-line)
       (progn 
         (close src-fs)
         (close dst-fs)
         nil))
      (let ((split-at (position #\% line)))
        (if split-at
          (format dst-fs "~A~%" (subseq line 0 split-at))
          (format dst-fs "~A~%" line))))))

;;; exercise 7-4
(defun print-2a (arr)
  (let ((rows (first (array-dimensions arr)))
        (cols (second (array-dimensions arr))))
    (dotimes (y rows)
      (dotimes (x cols)
        (format t "~10,2,0,'*,' F | " (aref arr y x)))
      (terpri))))

;;; exercise 7-5


;;; exercise 7-6

