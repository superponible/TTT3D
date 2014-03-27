; io.lsp

; these functions have to do with getting input from 
;   the user or outputting to the screen

; prints instructions
(defun ask-instructions ()
  (prints "Welcome to 3D Tic-Tac-Toe.")
  (prints "")
  (prints "Would you like instructions (Y or N)?")
  (setf inst (read))
  (if (eq inst 'N) (return-from ask-instructions nil))

  (clear-screen)
  (prints "Answer the questions after these instructions, then begin playing.")
  (prints "")
  (prints "The game board is displayed as follows:")
  (print-board (setf temp-board (create-board)))
  (prints "")
  (prints "Enter your moves in the form (plane row col).  For example, if ")
  (prints "you were X, a move (1 2 3) would put an X as follows:")
  (setf (aref temp-board 0 1 2) 'X)
  (print-board temp-board)
  (prints "")
  (prints "Hit enter to begin playing.")
  (read-line)
  (clear-screen)
)

; prints the game board giving the coordinates of each point
(defun print-board (board)
  (setf line nil)
  (format t "~%")
  (format t "plane:      1         2         3         4~%")
  (dotimes (j 4 line)
    (if (eq j 1) (format t "row  : ~a " (+ j 1)) (format t "       ~a " (+ j 1)))
    (dotimes (k 4 line)
      (dotimes (i 4 line)
        (setf line (append line (list (aref board k j i)))) 
      )
      (if (not (eq k 3)) (setf line (append line (list '*))))
    )
    (printl line)
    (setf line nil)
  )
  (format t "col  :   1 2 3 4   1 2 3 4   1 2 3 4   1 2 3 4~%")
)

; shortcut function to print a string
(defun prints (txt)
  (format t "~a~%" txt)
)

; prints a list without the parentheses -- from the shrink.lsp program
(defun printl (message)
  (mapcar #'(lambda (txt) (format t "~a " txt))
          message)
  (terpri)
)

; outputs 30 new lines to clear the screen
(defun clear-screen ()
  (dotimes (n 30 nil)
    (format t "~%")
  )
)

; based on input from the player, determines who will go first and second
(defun first-second ()
  (format t "~%Would you like to go first or second (F or S)?~%")
  (setf first (read))
  (cond ((eq first 'F) (setf first 'him))
        ((eq first 'S) (setf first 'me))
        (t (format t "~%Bad answer.  I get to go first.~%") (setf first 'me))
  )
)

; based on input from the player, determines who will be X and O
(defun pick-side ()
  (format t "~%Would you like to be X or O?~%")
  (setf side (read))
  (cond ((or (eq side 'X) (eq side 'O)) (setf side side))
        (t (format t "~%Bad answer.  I'll be X.~%") (setf side 'O))
  )
)
