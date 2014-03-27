; create.lsp

; contains functions that hold arrays and lookup tables used
;   throughout the program, and other functions used to get
;   the values for the arrays

; used to create the initial game board
(defun create-board ()
  (setf board (make-array '(4 4 4)
                          :initial-contents
                          '(((- - - -) 
                             (- - - -) 
                             (- - - -) 
                             (- - - -))
                            ((- - - -) 
                             (- - - -) 
                             (- - - -) 
                             (- - - -))
                            ((- - - -) 
                             (- - - -) 
                             (- - - -) 
                             (- - - -))
                            ((- - - -) 
                             (- - - -) 
                             (- - - -) 
                             (- - - -))
                           )
              )
  )
)
; all the possible winning rows -- each list is the 4 points
;   that make up the row
(defun create-winners ()
  (setf winning-rows (make-array '(76 4)
                         :initial-contents
                         '(( 0  1  2  3)
                           ( 4  5  6  7)
                           ( 8  9 10 11)
                           (12 13 14 15)
                           (16 17 18 19)
                           (20 21 22 23)
                           (24 25 26 27)
                           (28 29 30 31)
                           (32 33 34 35)
                           (36 37 38 39)
                           (40 41 42 43)
                           (44 45 46 47)
                           (48 49 50 51)
                           (52 53 54 55)
                           (56 57 58 59)
                           (60 61 62 63)
                           ( 0  4  8 12)
                           ( 1  5  9 13)
                           ( 2  6 10 14)
                           ( 3  7 11 15)
                           (16 20 24 28)
                           (17 21 25 29)
                           (18 22 26 30)
                           (19 23 27 31)
                           (32 36 40 44)
                           (33 37 41 45)
                           (34 38 42 46)
                           (35 39 43 47)
                           (48 52 56 60)
                           (49 53 57 61)
                           (50 54 58 62)
                           (51 55 59 63)
                           ( 0  5 10 15)
                           ( 3  6  9 12)
                           (16 21 26 31)
                           (19 22 25 28)
                           (32 37 42 47)
                           (35 38 41 44)
                           (48 53 58 63)
                           (51 54 57 60)
                           ( 0 16 32 48)
                           ( 1 17 33 49)
                           ( 2 18 34 50)
                           ( 3 19 35 51)
                           ( 4 20 36 52)
                           ( 5 21 37 53)
                           ( 6 22 38 54)
                           ( 7 23 39 55)
                           ( 8 24 40 56)
                           ( 9 25 41 57)
                           (10 26 42 58)
                           (11 27 43 59)
                           (12 28 44 60)
                           (13 29 45 61)
                           (14 30 46 62)
                           (15 31 47 63)
                           ( 0 17 34 51)
                           ( 4 21 38 55)
                           ( 8 25 42 59)
                           (12 29 46 63)
                           ( 3 18 33 48)
                           ( 7 22 37 52)
                           (11 26 41 56)
                           (15 30 45 60)
                           ( 0 20 40 60)
                           ( 1 21 41 61)
                           ( 2 22 42 62)
                           ( 3 23 43 63)
                           (12 24 36 48)
                           (13 25 37 49)
                           (14 26 38 50)
                           (15 27 39 51)
                           ( 0 21 42 63)
                           ( 3 22 41 60)
                           (12 25 38 51)
                           (15 26 37 48)
                          )
                     )
    )
)

; each list is a list of the rows in winning-rows that is affected
;  when a move is made in the point on the board with index i
(defun create-map ()
  (setf contained-rows (make-array 64 
                                  :initial-contents
                                  '((0 16 32 40 56 64 72) 
                                    (0 17 41 65) 
                                    (0 18 42 66) 
                                    (0 19 33 43 60 67 73)
                                    (1 16 44 57) 
                                    (1 17 32 45) 
                                    (1 18 33 46) 
                                    (1 19 47 61) 
                                    (2 16 48 58)
                                    (2 17 33 49) 
                                    (2 18 32 50) 
                                    (2 19 51 62) 
                                    (3 16 33 52 59 68 74)
                                    (3 17 53 69) 
                                    (3 18 54 70) 
                                    (3 19 32 55 63 71 75)
                                    (4 20 34 40)
                                    (4 21 41 56) 
                                    (4 22 42 60) 
                                    (4 23 35 43) 
                                    (5 20 44 64)
                                    (5 21 34 45 57 65 72)
                                    (5 22 35 46 61 66 73)
                                    (5 23 47 67)
                                    (6 20 48 68)
                                    (6 21 35 49 58 69 74)
                                    (6 22 34 50 62 70 75)
                                    (6 23 51 71)
                                    (7 20 35 52)
                                    (7 21 53 59) 
                                    (7 22 54 63)
                                    (7 23 34 55)
                                    (8 24 36 40)
                                    (8 25 41 60)
                                    (8 26 42 56)
                                    (8 27 37 43)
                                    (9 24 44 68)
                                    (9 25 36 45 61 69 75)
                                    (9 26 37 46 57 70 74)
                                    (9 27 47 71)
                                    (10 24 48 64)
                                    (10 25 37 49 62 65 73)
                                    (10 26 36 50 58 66 72)
                                    (10 27 51 67)
                                    (11 24 37 52)
                                    (11 25 53 63)
                                    (11 26 54 59)
                                    (11 27 36 55)
                                    (12 28 38 40 60 68 75)
                                    (12 29 41 69)
                                    (12 30 42 70)
                                    (12 31 39 43 56 71 74)
                                    (13 28 44 61)
                                    (13 29 38 45)
                                    (13 30 39 46)
                                    (13 31 47 57)
                                    (14 28 48 62)
                                    (14 29 39 49)
                                    (14 30 38 50)
                                    (14 31 51 58)
                                    (15 28 39 52 63 64 73)
                                    (15 29 53 65)
                                    (15 30 54 66)
                                    (15 31 38 55 59 67 72)
                                   )
                      )
  )                                  
)

; each list contains all the points on the board whose values need to be updated
;   when a move with index i has been made
(defun create-points ()
  (setf contained-points 
    (make-array 64 :initial-contents
	'((0 1 2 3 4 5 8 10 12 15 16 17 20 21 32 34 40 42 48 51 60 63)
	  (0 1 2 3 5 9 13 17 21 33 41 49 61)
	  (0 1 2 3 6 10 14 18 22 34 42 50 62)
	  (0 1 2 3 6 7 9 11 12 15 18 19 22 23 33 35 41 43 48 51 60 63)
	  (0 4 5 6 7 8 12 20 21 36 38 52 55) 
          (0 1 4 5 6 7 9 10 13 15 21 37 53)
	  (2 3 4 5 6 7 9 10 12 14 22 38 54) 
          (3 4 5 6 7 11 15 22 23 37 39 52 55)
	  (0 4 8 9 10 11 12 24 25 40 42 56 59)
	  (1 3 5 6 8 9 10 11 12 13 25 41 57) 
          (0 2 5 6 8 9 10 11 14 15 26 42 58)
	  (3 7 8 9 10 11 15 26 27 41 43 56 59)
	  (0 3 4 6 8 9 12 13 14 15 24 25 28 29 36 38 44 46 48 51 60 63)
	  (1 5 9 12 13 14 15 25 29 37 45 49 61)
	  (2 6 10 12 13 14 15 26 30 38 46 50 62)
	  (0 3 5 7 10 11 12 13 14 15 26 27 30 31 37 39 45 47 48 51 60 63)
	  (0 16 17 18 19 20 21 24 26 28 31 32 48)
	  (0 1 16 17 18 19 21 25 29 33 34 49 51)
	  (2 3 16 17 18 19 22 26 30 33 34 48 50)
	  (3 16 17 18 19 22 23 25 27 28 31 35 51)
	  (0 4 16 20 21 22 23 24 28 36 40 52 60)
	  (0 1 4 5 16 17 20 21 22 23 25 26 29 31 37 38 41 42 53 55 61 63)
	  (2 3 6 7 18 19 20 21 22 23 25 26 28 30 37 38 41 42 52 54 60 62)
	  (3 7 19 20 21 22 23 27 31 39 43 55 63)
	  (8 12 16 20 24 25 26 27 28 36 40 48 56)
	  (8 9 12 13 17 19 21 22 24 25 26 27 28 29 37 38 41 42 49 51 57 59)
	  (10 11 14 15 16 18 21 22 24 25 26 27 30 31 37 38 41 42 48 50 56 58)
	  (11 15 19 23 24 25 26 27 31 39 43 51 59)
	  (12 16 19 20 22 24 25 28 29 30 31 44 60)
	  (12 13 17 21 25 28 29 30 31 45 46 61 63)
	  (14 15 18 22 26 28 29 30 31 45 46 60 62)
	  (15 16 19 21 23 26 27 28 29 30 31 47 63)
	  (0 16 32 33 34 35 36 37 40 42 44 47 48)
	  (1 3 17 18 32 33 34 35 37 41 45 48 49)
	  (0 2 17 18 32 33 34 35 38 42 46 50 51)
	  (3 19 32 33 34 35 38 39 41 43 44 47 51)
	  (4 12 20 24 32 36 37 38 39 40 44 48 52)
	  (5 7 13 15 21 22 25 26 32 33 36 37 38 39 41 42 45 47 48 49 52 53)
	  (4 6 12 14 21 22 25 26 34 35 36 37 38 39 41 42 44 46 50 51 54 55)
	  (7 15 23 27 35 36 37 38 39 43 47 51 55)
	  (0 8 20 24 32 36 40 41 42 43 44 56 60)
	  (1 3 9 11 21 22 25 26 33 35 37 38 40 41 42 43 44 45 56 57 60 61)
	  (0 2 8 10 21 22 25 26 32 34 37 38 40 41 42 43 46 47 58 59 62 63)
	  (3 11 23 27 35 39 40 41 42 43 47 59 63)
	  (12 28 32 35 36 38 40 41 44 45 46 47 60)
	  (13 15 29 30 33 37 41 44 45 46 47 60 61)
	  (12 14 29 30 34 38 42 44 45 46 47 62 63)
	  (15 31 32 35 37 39 42 43 44 45 46 47 63)
	  (0 3 12 15 16 18 24 26 32 33 36 37 48 49 50 51 52 53 56 58 60 63)
	  (1 13 17 25 33 37 48 49 50 51 53 57 61)
	  (2 14 18 26 34 38 48 49 50 51 54 58 62)
	  (0 3 12 15 17 19 25 27 34 35 38 39 48 49 50 51 54 55 57 59 60 63)
	  (4 7 20 22 36 37 48 52 53 54 55 56 60)
	  (5 21 37 48 49 52 53 54 55 57 58 61 63)
	  (6 22 38 50 51 52 53 54 55 57 58 60 62)
	  (4 7 21 23 38 39 51 52 53 54 55 59 63)
	  (8 11 24 26 40 41 48 52 56 57 58 59 60)
	  (9 25 41 49 51 53 54 56 57 58 59 60 61)
	  (10 26 42 48 50 53 54 56 57 58 59 62 63)
	  (8 11 25 27 42 43 51 55 56 57 58 59 63)
	  (0 3 12 15 20 22 28 30 40 41 44 45 48 51 52 54 56 57 60 61 62 63)
	  (1 13 21 29 41 45 49 53 57 60 61 62 63)
	  (2 14 22 30 42 46 50 54 58 60 61 62 63)
	  (0 3 12 15 21 23 29 31 42 43 46 47 48 51 53 55 58 59 60 61 62 63)
	 )
    )
  )
)

; used to generate the initial-contents of create-map function
(defun generate-map ()
  (setf winning-rows (create-winners))
  (setf contained-map (make-array 64 :initial-element ()))
  (dotimes (i 64 contained-map)
    (dotimes (j 76 nil)
      (dotimes (k 4 nil)
        (if (= i (aref winning-rows j k)) 
          (setf (aref contained-map i) (append (aref contained-map i) (list j))) 
        )
      )
    )
  )
)

; used to generate the initial-contents of the create-points-function
(defun contained-rows-to-point (rows)
  (setf point-list ())
  (dotimes (i 64 point-list)
    (setf row-list (aref rows i))
    (setf point-n-list ())
    (do
      ()
      ((null row-list) nil)
      (setf x (car row-list))
      (dotimes (y 4 nil)
        (setf point-n-list (append point-n-list (list (aref winning-rows x y))))
      )
      (setf row-list (cdr row-list))
    )
    (setf point-n-list (sort point-n-list #'<))
    (setf point-list (append point-list (list point-n-list)))
  )
)