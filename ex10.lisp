;;; exercise 10-1
(setf x 'a)
(setf y 'b)
(setf z '(c d))
`(,z ,x z)
`(x ,y ,@z)
`((,@z ,x) z)

