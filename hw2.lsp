;Homework 2 ;
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
; Question 1 ;
;;;;;;;;;;;;;;

; TODO: comment code
(defun BFS (FRINGE)
  (cond
   ((null FRINGE) '())
   ;return an empty list if empty
   ((atom (car FRINGE)) 
    (cons (car FRINGE) (BFS (cdr FRINGE)))
   )
   ;if the first element of current list is an object,
   ;then add it to the result list and continue BFS for the rest of the list.
   (t
    (BFS (append (cdr FRINGE) (car FRINGE)))
   ) 
   ;otherwise, if it is a list for default, it will append the element list to the end of current list according to the 
   ;algorithm of BFS for FIFO queue.
  )
)

;(print (BFS '(ROOT)))
;(print (BFS '((((L E) F) T))))
;(print (BFS '((R (I (G (H T)))))))
;(print (BFS '(((A (B)) C (D)))))
;(print (BFS '((T (H R E) E))))
;(print (BFS '((A ((C ((E) D)) B)))))
;;;;;;;;;;;;;;
; Question 2 ;
;;;;;;;;;;;;;;


; These functions implement a depth-first solver for the homer-baby-dog-poison
; problem. In this implementation, a state is represented by a single list
; (homer baby dog poison), where each variable is T if the respective entity is
; on the west side of the river, and NIL if it is on the east side.
; Thus, the initial state for this problem is (NIL NIL NIL NIL) (everybody 
; is on the east side) and the goal state is (T T T T).

; The main entry point for this solver is the function DFS, which is called
; with (a) the state to search from and (b) the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, DFS returns NIL.
; To call DFS to solve the original problem, one would call 
; (DFS '(NIL NIL NIL NIL) NIL) 
; However, it should be possible to call DFS with a different initial
; state or with an initial path.

; First, we define the helper functions of DFS.

; FINAL-STATE takes a single argument S, the current state, and returns T if it
; is the goal state (T T T T) and NIL otherwise.
(defun FINAL-STATE (S)
 (cond
  ((equal S '(T T T T)) T)
  (t nil)
  )
)

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), and which entity
; to move (A, equal to h for homer only, b for homer with baby, d for homer 
; with dog, and p for homer with poison). 
; It returns a list containing the state that results from that move.
; If applying this operator results in an invalid state (because the dog and baby,
; or poisoin and baby are left unsupervised on one side of the river), or when the
; action is impossible (homer is not on the same side as the entity) it returns NIL.
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((NIL NIL T T)).
(defun NEXT-STATE (S A)
  (cond
   ((equal A 'h);homer moves by himself
    (cond
     ((or (equal (second S) (third S)) (equal (second S) (fourth S))) nil)
     ;if homer moves by himself and the baby is on the same side as homer, 
     ;then we need to check if poision/dog is on the same side as baby.
     ;if they are with baby, after homer moves, the process fails and return nil.
     (t (list (cons (not (car S))(cdr S))))
     ;otherwise, it is a valid move. Update the homer status.
    )
   )
   
   ((equal A 'b);homer moves with baby
    (cond 
     ((equal (first S) (second S)) ; if homer on the same side as baby
      (list (list (not (first S)) (not (second S)) (third S) (fourth S))))
     (t nil); invalid state if they are not
     )
   
   )
   ((equal A 'd); homer moves with dog
    (cond 
     ((not (equal (first S) (third S))) nil)     
     ;check if homer and dog are on the same side, if not then invalid.
     ((equal (second S) (fourth S)) nil)
     ;if baby and poision are on the same side, it is invalid.
     (t (list (list (not (first S)) (second S) (not (third S)) (fourth S))))
     ;otherwise valid, update the status.
    )

   )
   ((equal A 'p);homer with poision
    (cond
     ((not (equal (first S) (fourth S))) nil)
     ;check if they are on the same side
     ((equal (second S) (third S)) nil)
     ;if baby and dog are on the same side, gg.
     (t (list (list (not (first S)) (second S) (third S) (not (fourth S)))))
     ;otherwise we are good to go and update the status.
    )
   )
   (t nil)
  )
)

;(print (NEXT-STATE '(T T T T) 'h))
;(print (NEXT-STATE '(T T T T) 'b))
;(print (NEXT-STATE '(nil T nil nil) 'd))
;(print (NEXT-STATE '(T T nil T) 'p))
;(print (NEXT-STATE '(T nil T T) 'h))

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun SUCC-FN (S)
  (append (NEXT-STATE S 'h) (NEXT-STATE S 'b) (NEXT-STATE S 'd) (NEXT-STATE S 'p))
)

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (S) and the
; stack of states visited by DFS (STATES). It returns T if s is a member of
; states and NIL otherwise.
(defun ON-PATH (S STATES)
  (cond 
   ((null STATES) nil);return null if STATES is an empty list
   ((equal S (car STATES)) T);return T if found
   (t (ON-PATH S (cdr STATES)))
   ;if not then do it recursively for the rest of the list until it reaches the end of the list.
  )
)

; MULT-DFS is a helper function for DFS. It takes two arguments: a list of
; states from the initial state to the current state (PATH), and the legal
; successor states to the last, current state in the PATH (STATES). PATH is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a depth-first search on each element of STATES in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL.
(defun MULT-DFS (STATES PATH)
  (cond
   ((null STATES) nil)
   ;return nil if no more possible states avaliable.
   ((null (DFS (car STATES) PATH)) (MULT-DFS (cdr STATES) PATH))
   ;do dfs for the first possible state. If it returns null, try the rest possible states recursively until no more possible
   ;states avaliable.
   ;if none of them returns something other than nil, then it returns nil up to current branch.
   (t (DFS (car STATES) PATH))
   ;if it returns something, then track to the bottom leaf.
  )
)

; DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH is set to NIL. DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun DFS (S PATH)
  (cond
   ((ON-PATH S PATH) NIL)
   ; check if there is a revisiting.
   ((FINAL-STATE S) (append PATH (list S)))
   ;if S is the final state, which is (T T T T), then we append S to Path and return.
   (t (MULT-DFS (SUCC-FN S) (append PATH (list S))))
   ;otherwise add S to the path and call the helper function to find for the next successful state by using succ-fn 
  )
)
    
(print (DFS '(nil nil nil nil) nil))
(print (DFS '(T T T nil) nil))
(print (DFS '(T T nil nil) nil))
(print (DFS '(nil nil T nil) nil))
(print (DFS '(T nil T nil) nil))
