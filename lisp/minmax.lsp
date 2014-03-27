; minmax.lsp

; main function and helper for the 2 level minmax algorithm

; sets initial values and calls helper
(defun make-move-minmax (board player-side number-of-moves)
  (setf my-side (other-side player-side)
        current-ply 1 
        ab-prune '()
        max-plies 2
  )

  (make-move-minmax-aux board current-ply)
)

; makes a move using minmax
;  current-ply even is player moving, odd is computer
(defun make-move-minmax-aux (board current-ply) 
        ; above leaf level
  (cond ((< current-ply max-plies)
         (setf listtable ()
               point 0
               piece-to-place (if (oddp current-ply) my-side player-side)
         )
         ; recursive call after making an intial move
         ;   gets "best" move at next lowest level and puts it into listtable
         (dotimes (k 4 nil)
           (dotimes (j 4 nil)
             (dotimes (i 4 nil)
               (cond ((eq (aref board k j i) '-)
                       (setf (aref board k j i) piece-to-place)
                       (setf listtable (append listtable (list (make-move-minmax-aux board (+ current-ply 1)))))
                       (setf (aref board k j i) '-) 
                     )
                     (t (setf listtable (append listtable '((x x)))))
               )
               (setf point (+ point 1))
             )
           )
         )
         
	 ; from the best moves at this level, a random one is returned
         (setf loc-val (random-select-val (if (oddp current-ply) (max-list listtable current-ply) (min-list listtable current-ply))))
        )
        ; leaf level
        ((eq current-ply max-plies)
         (setf table (make-array 64)
               point 0
               value (current-board board player-side)
               ab-function (if (oddp current-ply) #'> #'<)
         )
         ; make a move and get the heuristic value after it
         (dotimes (k 4 nil)
           (dotimes (j 4 nil)
             (dotimes (i 4 nil)
               (cond ((eq (aref board k j i) '-)
                      (setf (aref table point) (table-value board value point player-side current-ply))
                      ; if statement handles alpha-beta pruning
                      (if (not (null ab-prune)) 
                        (if (funcall ab-function (aref table point) ab-prune)
                          (return-from make-move-minmax-aux (list point (aref table point)))
                        )
                      )
                     )
                     (t (setf (aref table (coord-to-point k j i)) '-))
               )
               (setf point (+ point 1))
             )
           )
         )
        
	 ; from the array of moves, pick the best ones, and return a random one if more than one
         ;  update ab-prune value if necessary
         (setf loc-val (random-select-val (if (oddp current-ply) (max-array table 64 current-ply) (min-array table 64 current-ply)))
               ab-prune (cadr loc-val)
         )
         loc-val
        )
  )
)
