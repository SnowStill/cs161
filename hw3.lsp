;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
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
);end defun
;test goal test
;(print (goal-test '2))
;(print (goal-test '(0 1)))
;(print (goal-test '((0 0 0 1 1 1 1 0 0) (1 1 1 1 0 0 1 1 0) (1 0 0 0 2 0 0 1 0) (1 0 0 5 5 5 0 1 0)
;(1 0 0 4 0 4 0 1 1)(1 1 0 5 0 5 0 0 1)(0 1 1 5 5 5 0 0 1)(0 0 1 0 2 0 1 1 1)(0 0 1 0 3 0 1 0 0)
;(0 0 1 1 1 1 1 0 0))))


; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;
(defun get-square (s r c)
  (cond
   ;if null return the wall value since it was not foud
   ((null s) 1)
   ;return wall value if r or c is greater than the maxrc.
   ((or (> r (- (length s) 1)) (> c (- (length (car s)) 1))) 1);
   ;when r is greater than 0, eliminate the row from s and do the recursion for the rest of S.
   ;and reduce r by 1
   ((> r 0) (get-square (cdr s) (- r 1) c))
   ((= r 0) ; in the row where the elment at
    (cond  
     ((> c 0) (get-square(cons (cdr (car s)) (cdr s)) r (- c 1)));if column is non zero then get rid off the first 
     ;element of this row and reduce c by 1 until it recches 0.
     ((= c 0) (car (car s))) ; found the element
     (t 1) ; out of scope value
    )
   )
   (t 1)
  )
)

;(print (get-square '((1 2 3) (4 5 6) (7 8 9)) '2 '2))
;(print (get-square '((1 2 3) (4 5 6) (7 8 9)) '1 '2))
;(print (get-square '((1 2 3) (4 5 6) (7 8 9)) '5 '5))

(defun set-square (s r c v)
  (cond 
   ((null s) nil)
   ((or (> r (- (length s) 1)) (> c (- (length (car s)) 1))) nil)
   ((> r 0) (cons (car s) (set-square (cdr s) (- r 1) c v))) ; to the next row
   ((= r 0);on the row
    (cond
     ((= c 0)
      (cons (cons v (cdr (car s))) (cdr s))
     );found the element replace the term with v
     ((> c 0) 
      (cons ;iterate rows
       (cons;iterate columns 
	(car (car s)) ;first item of first row
	(car (set-square (cons (cdr (car s)) (cdr s)) r (- c 1) v))
       ); do the next recursion with  the rest s without the first item of first row
        ;after done with recursion we only cons back this row only, cuz we have rest info in the following cdr s.
       (cdr s)
      )
     ) 
     (t nil)
     )
    )
   (t nil) 
   )
)

;(print (set-square '((1 2 3) (4 5 6) (7 8 9)) '2 '2 'V))
;(print (set-square '((1 2 3) (4 5 6) (7 8 9)) '5 '6 'V))
;(print (set-square '((1 2 3) (4 5 6) (7 8 9)) '0 '0 'V))

;this function returns s after the keeper left his currnt blobk.
(defun after-move (s r c)
  (cond
   ((null s) nil)
   ;if it is just keeper, then this spot becomes blank
    ((= (get-square s r c) keeper) (set-square s r c blank))
    ;else if it is a keeperstar,then this spot becomes star
    ((= (get-square s r c) keeperstar) (set-square s r c star))
    (t nil)
  )
)

(defun try-move (s dir)
  (let* ((pos (getKeeperPosition s 0)) (c (car pos)) (r (cadr pos)));get keeper's position
    (cond
     ;move up
     ((equal dir 'UP)
      (let* ((one (get-square s (- r 1) c)) (two (get-square s (- r 2) c))) ;check the two block the keeper faces
	(cond
	 ((isBlank one) ;move keeper if it is just blank
	  (set-square (after-move s r c) (- r 1) c keeper)
	 )
	 ((isStar one) ;move keeper if it is a star and set to keeper star
	  (set-square (after-move s r c) (- r 1) c keeperstar)
	 )
	 ((and (isBox one) (isBlank two));move if it is box and blank, update the pos of keeper and box
	  (set-square (set-square (after-move s r c) (- r 1) c keeper) (- r 2) c box)
	 )
	 ((and (isBox one) (isStar two));move if it is box and star, update the pos of keeper and box 
					;and set new box to boxstar
	  (set-square(set-square (after-move s r c) (- r 1) c keeper) (- r 2) c boxstar)
	 )
	 ((and (isBoxStar one) (isBlank two));move if it is starbox and blank, update the pos of keeper and box
					     ;and set the new boxstar to box   
	  (set-square (set-square (after-move s r c) (- r 1) c keeperstar) (- r 2) c box)
	 )
	 ((and (isBoxStar one) (isStar two));move if it is starbox and star, update the pos of keeper and box
					    ;ans boxstar is still boxstar
	  (set-square (set-square (after-move s r c) (- r 1) c keeperstar) (- r 2) c boxstar)
	 )
	 (t nil)
	)
       )
      )
     ;move down
     ((equal dir 'DOWN)
      (let* ((one (get-square s (+ r 1) c)) (two (get-square s (+ r 2) c))) ;check the two block the keeper faces
        (cond
         ((isBlank one) ;move keeper if it is just blank
          (set-square (after-move s r c) (+ r 1) c keeper)
         )
         ((isStar one) ;move keeper if it is a star and set to keeper star
          (set-square (after-move s r c) (+ r 1) c keeperstar)
         )
         ((and (isBox one) (isBlank two));move if it is box and blank, update the pos of keeper and box
          (set-square (set-square (after-move s r c) (+ r 1) c keeper) (+ r 2) c box)
         )
         ((and (isBox one) (isStar two));move if it is box and star, update the pos of keeper and box
                                        ;and set new box to boxstar
          (set-square(set-square (after-move s r c) (+ r 1) c keeper) (+ r 2) c boxstar)
         )
         ((and (isBoxStar one) (isBlank two));move if it is starbox and blank, update the pos of keeper and box
                                             ;and set the new boxstar to box
          (set-square (set-square (after-move s r c) (+ r 1) c keeperstar) (+ r 2) c box)
         )
	 ((and (isBoxStar one) (isStar two));move if it is starbox and star, update the pos of keeper and box
                                            ;ans boxstar is still boxstar
          (set-square (set-square (after-move s r c) (+ r 1) c keeperstar) (+ r 2) c boxstar)
         )
         (t nil)
        )
       )
     )
     ;move left
     ((equal dir 'LEFT)
      (let* ((one (get-square s r (- c 1))) (two (get-square s r (- c 2)))) ;check the two block the keeper faces
        (cond
         ((isBlank one) ;move keeper if it is just blank
          (set-square (after-move s r c) r (- c 1) keeper)
         )
         ((isStar one) ;move keeper if it is a star and set to keeper star
          (set-square (after-move s r c) r (- c 1) keeperstar)
         )
         ((and (isBox one) (isBlank two));move if it is box and blank, update the pos of keeper and box
          (set-square (set-square (after-move s r c) r (- c 1) keeper) r (- c 2) box)
         )
         ((and (isBox one) (isStar two));move if it is box and star, update the pos of keeper and box
                                        ;and set new box to boxstar
          (set-square(set-square (after-move s r c) r (- c 1) keeper) r (- c 2) boxstar)
         )
         ((and (isBoxStar one) (isBlank two));move if it is starbox and blank, update the pos of keeper and box
                                             ;and set the new boxstar to box
          (set-square (set-square (after-move s r c) r (- c 1) keeperstar) r (- c 2) box)
         )
         ((and (isBoxStar one) (isStar two));move if it is starbox and star, update the pos of keeper and box
                                            ;ans boxstar is still boxstar
          (set-square (set-square (after-move s r c) r (- c 1) keeperstar) r (- c 2) boxstar)
         )
         (t nil)
        )
       )
     )
     ;move right
     ((equal dir 'RIGHT)
      (let* ((one (get-square s r (+ c 1))) (two (get-square s r (+ c 2)))) ;check the two block the keeper faces
        (cond
         ((isBlank one) ;move keeper if it is just blank
          (set-square (after-move s r c) r (+ c 1) keeper)
         )
         ((isStar one) ;move keeper if it is a star and set to keeper star
          (set-square (after-move s r c) r (+ c 1) keeperstar)
         )
         ((and (isBox one) (isBlank two));move if it is box and blank, update the pos of keeper and box
          (set-square (set-square (after-move s r c) r (+ c 1) keeper) r (+ c 2) box)
         )
         ((and (isBox one) (isStar two));move if it is box and star, update the pos of keeper and box
                                        ;and set new box to boxstar
          (set-square(set-square (after-move s r c) r (+ c 1) keeper) r (+ c 2) boxstar)
         )
         ((and (isBoxStar one) (isBlank two));move if it is starbox and blank, update the pos of keeper and box
                                             ;and set the new boxstar to box
          (set-square (set-square (after-move s r c) r (+ c 1) keeperstar) r (+ c 2) box)
         )
         ((and (isBoxStar one) (isStar two));move if it is starbox and star, update the pos of keeper and box
                                            ;ans boxstar is still boxstar
          (set-square (set-square (after-move s r c) r (+ c 1) keeperstar) r (+ c 2) boxstar)
         )
         (t nil)
	 )
	)
      )
     )
    )
)
;blank test
;(print (try-move '((0 0 0) (0 0 0) (0 3 0)) 'UP))
;move from keeper+goal test
;(print (try-move '((0 0 0) (0 0 0) (0 6 0)) 'UP))
;box-blank test
;(print (try-move '((0 0 0) (0 2 0) (0 3 0)) 'UP))
;box-star test
;(print (try-move '((0 4 0) (0 2 0) (0 3 0)) 'UP))
;box+goal-goal test and move from keeper+goal test
;(print (try-move '((0 4 0) (0 5 0) (0 6 0)) 'UP))
;box+goal- test
;(print (try-move '((0 0 0) (0 5 0) (0 3 0)) 'UP))
;box+goal-goal test
;(print (try-move '((0 4 0) (0 5 0) (0 3 0)) 'UP))
;wall, two boxes test
;(print (try-move '((0 2 0) (0 2 0) (0 3 0)) 'UP))
;(print (try-move '((0 6 0) (0 2 0) (0 3 0)) 'UP))
;(print (try-move '((0 2 0) (0 6 0) (0 3 0)) 'UP))
;(print (try-move '((0 4 0) (0 1 0) (0 3 0)) 'UP))
;(print (try-move '((0 6 0) (0 6 0) (0 3 0)) 'UP))

;blank test
;(print (try-move '((0 3 0) (0 0 0) (0 0 0)) 'DOWN))
;move from keeper+goal test
;(print (try-move '((0 6 0) (0 0 0) (0 0 0)) 'DOWN))
;box-blank test
;(print (try-move '((0 3 0) (0 2 0) (0 0 0)) 'DOWN))
;box-star test
;(print (try-move '((0 3 0) (0 2 0) (0 4 0)) 'DOWN))
;box+goal-goal test and move from keeper+goal test
;(print (try-move '((0 6 0) (0 5 0) (0 4 0)) 'DOWN))
;box+goal- test
;(print (try-move '((0 3 0) (0 5 0) (0 0 0)) 'DOWN))
;box+goal-goal test
;(print (try-move '((0 3 0) (0 5 0) (0 4 0)) 'DOWN))
;wall, two boxes test
;(print (try-move '((0 3 0) (0 2 0) (0 2 0)) 'DOWN))
;(print (try-move '((0 3 0) (0 2 0) (0 6 0)) 'DOWN))
;(print (try-move '((0 3 0) (0 6 0) (0 2 0)) 'DOWN))
;(print (try-move '((0 3 0) (0 1 0) (0 4 0)) 'DOWN))
;(print (try-move '((0 3 0) (0 6 0) (0 6 0)) 'DOWN))

;blank test
;(print (try-move '((0 0 0) (0 0 3) (0 0 0)) 'LEFT))
;move from keeper+goal test
;(print (try-move '((0 0 0) (0 0 6) (0 0 0)) 'LEFT))
;box-blank test
;(print (try-move '((0 0 0) (0 2 3) (0 0 0)) 'LEFT))
;box-star test
;(print (try-move '((0 0 0) (4 2 3) (0 0 0)) 'LEFT))
;box+goal-goal test and move from keeper+goal test
;(print (try-move '((0 0 0) (4 5 6) (0 0 0)) 'LEFT))
;box+goal- test
;(print (try-move '((0 0 0) (0 5 3) (0 0 0)) 'LEFT))
;box+goal-goal test
;(print (try-move '((0 0 0) (4 5 3) (0 0 0)) 'LEFT))
;wall, two boxes test
;(print (try-move '((0 0 0) (2 2 3) (0 0 0)) 'LEFT))
;(print (try-move '((0 0 0) (6 2 3) (0 0 0)) 'LEFT))
;(print (try-move '((0 0 0) (2 6 3) (0 0 0)) 'LEFT))
;(print (try-move '((0 0 0) (4 1 3) (0 3 0)) 'LEFT))
;(print (try-move '((0 0 0) (6 6 3) (0 0 0)) 'LEFT))

;blank test
;(print (try-move '((0 0 0) (3 0 0) (0 0 0)) 'RIGHT))
;move from keeper+goal test
;(print (try-move '((0 0 0) (6 0 0) (0 0 0)) 'RIGHT))
;box-blank test
;(print (try-move '((0 0 0) (3 2 0) (0 0 0)) 'RIGHT))
;box-star test
;(print (try-move '((0 0 0) (3 2 4) (0 0 0)) 'RIGHT))
;box+goal-goal test and move from keeper+goal test
;(print (try-move '((0 0 0) (6 5 4) (0 0 0)) 'RIGHT))
;box+goal- test
;(print (try-move '((0 0 0) (3 5 0) (0 0 0)) 'RIGHT))
;box+goal-goal test
;(print (try-move '((0 0 0) (3 5 4) (0 0 0)) 'RIGHT))
;wall, two boxes test
;(print (try-move '((0 0 0) (3 2 2) (0 0 0)) 'RIGHT))
;(print (try-move '((0 0 0) (3 2 6) (0 0 0)) 'RIGHT))
;(print (try-move '((0 0 0) (3 6 2) (0 0 0)) 'RIGHT))
;(print (try-move '((0 0 0) (3 1 4) (0 3 0)) 'RIGHT))
;(print (try-move '((0 0 0) (3 6 6) (0 0 0)) 'RIGHT))

(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (x (car pos))
	 (y (cadr pos))
	 ;x and y are now the coordinate of the keeper in s.
	 (result (list (try-move s 'UP) (try-move s 'DOWN) (try-move s 'LEFT) (try-move s 'RIGHT))) ;try moves in all four directions
	 )
    (cleanUpList result);end
    )
);

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
;(print (next-states '((0 0 0) (3 0 0) (0 0 0))))
;(print (next-states '((0 0 0) (0 3 0) (0 0 0))))

(defun h0 (s)
  0
  )

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;
;h1 is admissible heuristic. For each box not on the goal,
;at least one step is required. So it can take up to xN steps,
;so the cost of reaching goal is not going to overestimated.
(defun h1 (s)
  (cond 
   ((null s) 0);return 0 if s is null, which is also the basic case
   ((atom s) ;if s is an objecct
    (cond 
     ((isBox s) 1) ;if it is a box, return 1
     (t 0));else 0
    )
   (t ;otherwise it is stil a list, then continue the recursion.
    (+ (h1 (car s)) (h1 (cdr s)));sum the returned values
   )
  )
)

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;

;helper functions


;kinda copy and paste from the getkeeperposition
(defun getBoxColumn (r col)
  (cond ((null r) nil)
	(t (if (isBox (car r))
	       col
	     (getBoxColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
)

(defun getBoxPosition (s row) ;returns as (c r)
  (cond ((null s) nil)
      (t (let ((x (getBoxColumn (car s) 0)))
	   (if x
	       ;keeper is in this row
	       (list x row)
	       ;otherwise move on
	     (getBoxPosition (cdr s) (+ row 1))
	     );end if
	   );end let
	 );end t
      );end cond
);end defun

(defun getGoalColumn (r col)
  (cond ((null r) nil)
	(t (if (isStar (car r))
	       col
	     (getGoalColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
)

(defun getGoalPosition (s row) 
  (cond ((null s) nil)
	(t (let ((x (getGoalColumn (car s) 0)))
	     (if x
					;keeper is in this row
		 (list x row)
					;otherwise move on
	       (getGoalPosition (cdr s) (+ row 1))
	       );end if
	     );end let
	   );end t
	);end cond
);end defun


(defun h205356739 (s)

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are roughly ordered by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below 
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 | 
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must 
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
