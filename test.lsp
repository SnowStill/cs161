(setq box 2)

(defun isBox (v)
  (= v box)
  )

(defun goal-test (s)
  (cond
   ;return true if there are no more obejcts to check
   ((null s) T)
   ;if s is an object, not a list, check if it is a box or not
   ((atom s) (not (isBox s)))
   ;otherwise if it is a list, do goal-test recursively to check each indivisual objects/lists
   ;'and' it so it makes sure it fails the check if there is any box.
   (t
    (and (goal-test (car s)) (goal-test (cdr s)))
   )
  )
)

(print (goal-test '((0 0 0 1 1 1 1 0 0)
		        (1 1 1 1 0 0 1 1 0)
			    (1 0 0 0 0 0 0 1 0)
			        (1 0 0 5 5 5 0 1 0)
				    (1 0 0 4 0 4 0 1 1)
				        (1 1 0 5 0 5 0 0 1)
					    (0 1 1 5 5 5 0 0 1)
					        (0 0 1 0 0 0 1 1 1)
						    (0 0 1 0 3 0 1 0 0)
						        (0 0 1 1 1 1 1 0 0))))
