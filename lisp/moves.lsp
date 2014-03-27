; moves.lsp

; contains functions that are used to help make a computer move

; reads a move from the player in a list format
;   updates the board if the move is valid
(defun read-move (board player-side algo)
  (loop
    (format t "~%Make your move in the form (plane row column).~%")

    (setf his-move (read))

    ; something here to handle error if I/P w/o ()  ::  (if (not (listp his-move)) (list his-move))
    (cond ((not (check-move his-move board)) (format t "~%Invalid move.~%"))
          (t                                 (return (mapcar #'- his-move '(1 1 1))))
    )
  )
)

; checks if the move the player entered is a valid format:
;  length = 3,  1 <= i,j,k <= 4, 3 points are numbers, the spot is empty
(defun check-move (move board)
  (cond ((not (= (length move) 3)) (return-from check-move nil))
        ((not (equal (mapcar #'numberp move) '(t t t))) (return-from check-move nil)) 
        ((not (equal (mapcar #'< move '(5 5 5)) '(t t t))) (return-from check-move nil))
        ((not (equal (mapcar #'> move '(0 0 0)) '(t t t))) (return-from check-move nil))
        ((not (eq (aref board (- (car move) 1) (- (cadr move) 1) (- (caddr move) 1)) '-)) (return-from check-move nil))
        (t t)
  )
)

; uses the contained-rows to determine which rows need to be
;   recalculated based on the move made
;   only checking these rows speeds things up
;   places the current side's symbol then calls helper
(defun table-value (board value location player-side current-ply)
  (setf board-coord (point-to-coord location) 
        k (car board-coord) 
        j (cadr board-coord) 
        i (caddr board-coord)
        rows-to-check (aref contained-rows location)
        my-side (other-side player-side)
        side-to-place (if (oddp current-ply) my-side player-side)
  )
  (do ()
      ((null rows-to-check) value)
    (setf row (car rows-to-check))

    (setf value (table-value-aux row value -1 player-side my-side))

    (setf (aref board k j i) side-to-place)

    (setf value (table-value-aux row value 1 player-side my-side))
    
    (setf rows-to-check (cdr rows-to-check))
    (setf (aref board k j i) '-)
  )
  value
)

; table value helper - adds up the number of X's and O's in the given row
;   returns the proper board value for number of X's and O's
(defun table-value-aux (row value dec-or-inc player-side my-side)
  (setf comp-count 0 
        player-count 0
  )
  (dotimes (n 4 nil)
    (setf coord (point-to-coord (aref winning-rows row n)))
    (cond ((eq (aref board (car coord) (cadr coord) (caddr coord)) player-side) (setf player-count (+ player-count 1)))
          ((eq (aref board (car coord) (cadr coord) (caddr coord)) my-side) (setf comp-count (+ comp-count 1)))
    )
  )
  (cond ((and (eq player-count 0) (> comp-count 0)) (setf value (+ value (* (expt-lookup comp-count 'c) dec-or-inc))))
        ((and (eq comp-count 0) (> player-count 0)) (setf value (- value (* (expt-lookup player-count 'p) dec-or-inc))))
  )
  value
)

; offensive heuristic -- computer moves = 1.1 * player moves
(defun expt-lookup (n side)
  (if (eq side 'p)
    ; player's row
    (cond ((< n 1) 0)
          ((= n 1) 1)
          ((= n 2) 10)
          ((= n 3) 100)
          ((= n 4) 5000)
    )
    ; computer's row
    (cond ((< n 1) 0)
          ((= n 1) 1)
          ((= n 2) 11)
          ((= n 3) 110)
          ((= n 4) 5100)
    ) 
  )   
)

; called at the start of determining a move
;  gets the heuristic value of the entire board to use as a starting point
;    this way, the value only needs to be changes for the moves that are made
;    and doesnt need to be totally recomputed on each move
(defun current-board (board player-side)
  (setf my-side (other-side player-side)
        value 0
  )
  (dotimes (i 76 value)
    (setf comp-count 0
          player-count 0
    )
    (dotimes (j 4 value)
      (setf coord (point-to-coord (aref winning-rows i j)))
      (cond ((eq (aref board (car coord) (cadr coord) (caddr coord)) player-side) (setf player-count (+ player-count 1)))
            ((eq (aref board (car coord) (cadr coord) (caddr coord)) my-side) (setf comp-count (+ comp-count 1)))
      )
    ) 
    (cond ((and (eq player-count 0) (> comp-count 0)) (setf value (+ value (expt-lookup comp-count 'c))))
          ((and (eq comp-count 0) (> player-count 0)) (setf value (- value (expt-lookup player-count 'p))))
    ) 
  )
  value
)
