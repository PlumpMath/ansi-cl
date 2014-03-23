;;; exercise 8-4

(defpackage "RING-BUFFER"
  (:use "COMMON-LISP")
  (:nicknames "RING")
  (:export "NEW-BUF"
           "BUF-INSERT"
           "BUF-POP"
           "BUF-NEXT"
           "BUF-RESET"
           "BUF-CLEAR"
           "BUF-FLUSH"))

(defpackage "FILE-UTILITY"
  (:use "COMMON-LISP"
        "RING-BUFFER")
  (:nicknames "FILE")
  (:export "FILE-SUBST"
           "STREAM-SUBST"))

(in-package ring-buffer)

(defstruct buf
  vec (start -1) (used -1) (new -1) (end -1))

(defun bref (buf n)
  (svref (buf-vec buf)
         (mod n (length (buf-vec buf)))))

(defun (setf bref) (val buf n)
  (setf (svref (buf-vec buf)
               (mod n (length (buf-vec buf))))
        val))

(defun new-buf (len)
  (make-buf :vec (make-array len)))

(defun buf-insert (x b)
  (setf (bref b (incf (buf-end b))) x))

(defun buf-pop (b)
  (prog1
    (bref b (incf (buf-start b)))
    (setf (buf-used b) (buf-start b)
          (buf-new  b) (buf-end   b))))

(defun buf-next (b)
  (when (< (buf-used b) (buf-new b))
    (bref b (incf (buf-used b)))))

(defun buf-reset (b)
  (setf (buf-used b) (buf-start b)
        (buf-new  b) (buf-end   b)))

(defun buf-clear (b)
  (setf (buf-start b) -1 (buf-used b) -1
        (buf-new   b) -1 (buf-end  b) -1))

(defun buf-flush (b str)
  (do ((i (1+ (buf-used b)) (1+ i)))
    ((> i (buf-end b)))
    (princ (bref b i) str)))

(in-package file-utility)

(defun file-subst (old new file1 file2)
  (with-open-file (in file1 :direction :input)
    (with-open-file (out file2 :direction :output :if-exists :supersede)
      (stream-subst old new in out))))

(defun stream-subst (old new in out)
  (let* ((pos 0)
         (len (length old))
         (buf (new-buf len))
         (from-buf nil))
    (do ((c (read-char in nil :eof)
            (or (setf from-buf (buf-next buf))
                (read-char in nil :eof))))
      ((eql c :eof))
      (cond ((char= c (char old pos))
             (incf pos)
             (cond ((= pos len)
                    (princ new out)
                    (setf pos 0)
                    (buf-clear buf))
                   ((not from-buf)
                    (buf-insert c buf))))
            ((zerop pos)
             (princ c out)
             (when from-buf
               (buf-pop buf)
               (buf-reset buf)))
            (t
             (unless from-buf
               (buf-insert c buf))
             (princ (buf-pop buf) out)
             (buf-reset buf)
             (setf pos 0))))
    (buf-flush buf out)))

(in-package common-lisp-user)


;;; exercise 8-5
(defparameter *words* (make-hash-table :size 10000))

(defconstant maxword 100)

(defun read-text (pathname)
  (with-open-file (s pathname :direction :input)
    (let ((buffer (make-string maxword))
          (pos 0))
      (do ((c (read-char s nil :eof)
              (read-char s nil :eof)))
        ((eql c :eof))
        (if (or (alpha-char-p c) (char= c #\'))
          (progn
            (setf (aref buffer pos) c)
            (incf pos))
          (progn
            (unless (zerop pos)
              (see (intern (string-downcase
                             (subseq buffer 0 pos))))
              (setf pos 0))
            (let ((p (punc c)))
              (if p (see p)))))))))

(defun punc (c)
  (case c
    (#\. '|.|)
    (#\, '|,|)
    (#\; '|;|)
     (#\! '|!|)
     (#\? '|?|)))

  (let ((prev '|.|))
    (defun see (symb)
      (let ((pair (assoc symb (gethash prev *words*))))
        (if (null pair)
          (push (cons symb 1) (gethash prev *words*))
          (incf (cdr pair))))
      (setf prev symb)))

  (defun generate-text (n &optional (prev '|.|))
    (if (zerop n)
      (terpri)
      (let ((next (random-next prev)))
        (format t "~A " next)
        (generate-text (1- n) next))))

  (defun random-next (prev)
    (let* ((choices (gethash prev *words*))
           (i (random (reduce #'+ choices
                              :key #'cdr))))
      (dolist (pair choices)
        (if (minusp (decf i (cdr pair)))
          (return (car pair))))))

  (defun string->symbol (string)
    (let ((acc nil)
          (pos 0))
      (do ((buf (multiple-value-bind (v p)
                  (read-from-string string nil :eof :start pos)
                  (cons v p))
                (multiple-value-bind (v p)
                  (read-from-string string nil :eof :start pos)
                  (cons v p))))
        ((eql (car buf) :eof)
         (nreverse acc))
        (progn
          (setf pos (cdr buf))
          (push (car buf) acc)))))

  (defun match-current-and-next-p (current next)
    (maphash #'(lambda (words-k words-v)
                 (when (and (eql current words-k)
                            (assoc next words-v))
                   (return-from match-current-and-next-p t)))
             *words*))

  (defun from-words-p-loop (lst)
    (cond ((null lst)
           nil)
          ((match-current-and-next-p (first lst) (second lst))
           t)
          (t
           (from-words-p-loop (cdr lst)))))

  (defun from-words-p (string)
    (from-words-p-loop (string->symbol string)))


  ;;; exercise 8-6
