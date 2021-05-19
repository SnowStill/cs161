;problem 1
(defun PAD (N)
  (cond
   ; base case for the first 3 items, also where the recursion branch ends.
   ((<= N 2) 1)
   ;default/recursive case for the rest numbers, add all the trace-backed numbers.
   (t 
    (+ (PAD (- N 2)) (PAD (- N 3))))
   )
)
;problem 2
(defun SUMS (N)
  (cond
   ;base case for the first 3 items, also where the recursion branch ends.
   ((<= N 2) 0)
   ;default/recursive case for the rest numbers, adding one everytime it traces back a non-base number.
   (t
    (+ 1 (SUMS( - N 2)) (SUMS ( - N 3))))
   )
)

;problem 3
(defun ANON (TREE)
  (cond 
   ;check if it is a valid object
   ((not TREE) '())
   ;if it is a node/leaf, print the object by replacing ?
   ((atom TREE) '?)
   ;otherwise iterate the list recursively until it reaches the leaf, and consturct a list when the recursion traces back to the root 
   (t (cons (ANON (car TREE)) (ANON (cdr TREE))))
  )
)
