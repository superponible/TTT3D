; 3dttt.lsp

; main program file

; function to start the game
; optional parameter algo chooses which way to make move
;   0 - 2+ ply greedy algorithm only expanding a certain 
;           number of moves to next lower level (default)
;   1 - 2 ply minimax with alpha-beta pruning
(defun 3DTicTacToe (&optional (algo 0))
  (load "algos")
  (load "create")
  (load "greedy")
  (load "io")
  (load "minmax")
  (load "moves")
; (load "test")  ---- only needed for testing functions

  (clear-screen)

  (ask-instructions)

  ; main game loop -- keeps track of overall games record
  (setf record '(0 0 0))
  (loop
    ; play a game
    (setf score (play-game algo))
    ; update the record
    (setf record 
      (cond ((eq score 'won)  (list (+ (car record) 1) (cadr record) (caddr record)))
            ((eq score 'lost) (list (car record) (+ (cadr record) 1) (caddr record)))
            ((eq score 'draw) (list (car record) (cadr record) (+ (caddr record) 1)))
      )
    )
    ; ask for another game
    (format t "~%~a for me.  ~a for you.  ~a ~a.~%" 
      (car record) (cadr record) (caddr record) (if (= (caddr record) 1) "tie" "ties")
    )
    (format t "~%Would you like to play again (Y or N)?~%")
    (setf again (read))

    (cond ((eq again 'Y) ())
          ((eq again 'N) (return (printl '(OK.  Game over then.))))
          (t (return (printl (Bad answer.  Game over.))))
    )
  )
)

(defun play-game (algo)
  ; globals
  (setf board (create-board))
  (setf winning-rows (create-winners))  
  (setf contained-map (create-map))
  (setf contained-points (create-points))
  (setf decision-board (make-array 64 :initial-element 0)) 
 
  ; get info before playing
  (setf first-second (first-second))
  (setf player-side (pick-side))
  (setf my-side (other-side player-side))
  
  (print-board board)       ; show initial board
  (setf number-of-moves 0)

  ; if player goes 2nd, make a start move
  (cond ((eq first-second 'me) (format t "~%I'll make my move...~%")
                               (setf my-move 
                                 (point-to-coord 
                                   (car 
                                     (if (= algo 0) (make-move-greedy board player-side number-of-moves)
                                                    (make-move-minmax board player-side number-of-moves)
                                     )
                                   )
                                 )
                               )
                               (setf (aref board (car my-move) (cadr my-move) (caddr my-move)) my-side)
                               (print-board board)
                               (format t "~%My move was ~a.~%" (mapcar #'+ my-move '(1 1 1))) 
                               (setf number-of-moves (+ number-of-moves 1))
        )
  )

  (loop
    ; read a move from the player
    (setf his-move (read-move board player-side algo))
    (setf (aref board (car his-move) (cadr his-move) (caddr his-move)) player-side)

    (print-board board)
    ; check for win or draw
    (cond ((check-for-win board winning-rows) (format t "~%You won.~%")
                                              (return 'lost)
          )
          ((eq (setf number-of-moves (+ number-of-moves 1)) 64) (format t "~%Draw.~%")
                                                                (return 'draw)
          )                                                   
    )

    ; make computer move
    (format t "~%I'll make my move...~%")
    (setf my-move 
      (point-to-coord 
        (car 
          (if (= algo 0) (make-move-greedy board player-side number-of-moves)
                         (make-move-minmax board player-side number-of-moves)
          )
        )
      )
    )
    (setf (aref board (car my-move) (cadr my-move) (caddr my-move)) my-side)
    (print-board board)
    (format t "~%My move was ~a.~%" (mapcar #'+ my-move '(1 1 1)))
    ; check for win or draw
    (cond ((check-for-win board winning-rows) (format t "~%I won.~%")
                                              (return 'won)
          )
          ((eq (setf number-of-moves (+ number-of-moves 1)) 64) (format t "~%Draw.~%")
                                                                (return 'draw)
          )
    )    
  )
)

; compiles all the lsp files to object files, then loads the main program file
(defun compile-ttt ()
;  (compile-file "pro.lsp")
  (compile-file "algos.lsp")
  (compile-file "io.lsp")
  (compile-file "moves.lsp")
  (compile-file "minmax.lsp")
  (compile-file "greedy.lsp")
  (compile-file "create.lsp")
  (compile-file "tests.lsp")
  (load "3dttt")
)

(3DTicTacToe)
