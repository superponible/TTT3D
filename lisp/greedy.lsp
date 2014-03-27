; greedy.lsp

; contains functions for the greedy search algorithm

; make-move-greedy -- sets up initial values for greedy search
;   calls helper function
;
;   max-plies is the levels it searches to
;   total-size is the size of the list of moves to expand at each level
;   block-or-win makes an immediate move to a point if it will make 4 in
;     a row for computer at ply 1, or for the player at ply 2
(defun make-move-greedy (board player-side number-of-moves)
  (setf my-side (other-side player-side)
        current-ply 1
        value (current-board board player-side)
        ab-prune '()
        max-plies (if (< number-of-moves 5) 2 4) 
        total-size (if (< number-of-moves 5) 16 5)
        block-or-win '()        
  )

  (setf def-move (make-move-greedy-aux board current-ply value) )
  (if (null block-or-win) def-move block-or-win)
)

; make-move-greedy-aux -- helper function
(defun make-move-greedy-aux (board current-ply value)
  ; not leaf level of tree
  (cond ((< current-ply max-plies)
           (setf point 0
                 list-size 0
                 max-value 0
                 move-list '()
                 ltgt (if (oddp current-ply) #'> #'<)      ; > for max plies, < for min plies
           ) 
           ; loop thru the board and get the hueristic value of each empty space
           ;   place the new (point value) into the move-list if the list < 10 or
           ;   if the new value is greater than the min in max levels (current-ply odd)
           ;   or less than  the max in min levels (current-ply is even)
           (dotimes (k 4 nil)
             (dotimes (j 4 nil)
               (dotimes (i 4 nil)
                 (cond ((eq (aref board k j i) '-)
                        (setf current-value (table-value board value point player-side current-ply))
                        ; check if the move makes 4 in a row
                        (if (< current-ply 3)
                          (if (null block-or-win)
                            (if (or (< current-value -4000) (> current-value 4000))
                              (setf block-or-win (list point current-value))
                            )
                          )
                        )
                        (cond ((< list-size total-size)
                               (setf move-list (add-to-list (list point current-value) move-list list-size current-ply))
                               (setf list-size (+ list-size 1))
                              )
                              ((funcall ltgt current-value (cadar move-list))
                               (setf move-list (add-to-list (list point current-value) move-list list-size current-ply))
                               (setf list-size (+ list-size 1))
                              )
                        )
                       )
                 )
                 (setf point (+ point 1))
               )
             )
           )

           ; since not bottom level, call recursively with the 10 nodes to search on 
           ; make the move in the (point value) node, then call the function and get a
           ;   list from the next level
           (prog (cur-side-to-place cur-coord)
             (setf cur-side-to-place (if (oddp current-ply) my-side player-side)
                   cur-coord '(0 0 0)
                   best-moves '()
             )
             (dolist (elt move-list best-moves)
               (setf cur-coord (point-to-coord (car elt)))
               (setf (aref board (car cur-coord) (cadr cur-coord) (caddr cur-coord)) cur-side-to-place)
               (setf best-moves (append best-moves 
                                        (list (list (car elt) 
                                               (cadr (make-move-greedy-aux board (+ current-ply 1) (cadr elt)))
                                              ) 
                                        )
                                )
               )
               (setf (aref board (car cur-coord) (cadr cur-coord) (caddr cur-coord)) '-)
             )
           )

           ; get the "best" moves in the list of 10 for this level
           ; randomly select one to return
           (setf loc-val (random-select-val 
               (funcall (if (oddp current-ply) #'max-list-greedy #'min-list-greedy) best-moves current-ply)
                         ) 
           )
        )

        ; leaf level of tree
        ((= current-ply max-plies)
           (setf point 0
                 list-size 0
                 max-value 0
                 move-list '()
                 ltgt (if (oddp current-ply) #'> #'<)
                 ab-function (if (oddp current-ply) #'> #'<)
           ) 
           ; loop thru the board and get the hueristic value of each empty space
           ;   place the new (point value) into the move-list if the list < 10 or
           ;   if the new value is greater than the min in max levels (current-ply odd)
           ;   or less than  the max in min levels (current-ply is even)
           (dotimes (k 4 nil)
             (dotimes (j 4 nil)
               (dotimes (i 4 nil)
                 (cond ((eq (aref board k j i) '-)
                        (setf current-value (table-value board value point player-side current-ply))
                          ; if statement handles alpha-beta pruning
                          (if (not (null ab-prune)) 
                            (if (funcall ab-function current-value ab-prune)
                              (return-from make-move-greedy-aux (list point current-value))
                            )
                          )
                        (cond ((< list-size total-size)
                               (setf move-list (add-to-list (list point current-value) move-list list-size current-ply))
                               (setf list-size (+ list-size 1))
                              )
                              ((funcall ltgt current-value (cadar move-list))
                               (setf move-list (add-to-list (list point current-value) move-list list-size current-ply))
                               (setf list-size (+ list-size 1))
                              )
                        )
                       )
                 )
                 (setf point (+ point 1))
               )
             )
           )
        
           ; get the "best" moves in the list of 10 for this level
           ; randomly select one to return
           (setf loc-val 
                   (random-select-val 
                     (funcall (if (oddp current-ply) #'max-list-greedy #'min-list-greedy) move-list current-ply)
                   ) 
                 ab-prune (cadr loc-val)
           )
           loc-val
        )
  )
)