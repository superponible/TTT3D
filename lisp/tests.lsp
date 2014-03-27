; tests.lsp

; these functions are for testing other functions
;   they are not used anywhere in the program

; outputs a list of 76 T's if all the winning rows are checked correctly in check-for-win
; fills one of the winning rows with X's then checks the board for a win
;  does this for each row
(defun test-winners ()
  (setf winning-rows (create-winners))
  (setf thelist (make-array 76 :initial-element ()))
  (dotimes (i 76 nil)
    (setf board (create-board))
    (dotimes (j 4 nil)
      (setf coord (point-to-coord (aref winning-rows i j)))
      (setf (aref board (car coord) (cadr coord) (caddr coord)) 'X)
    )
    (print-board board)
    (setf (aref thelist i) (check-for-win board winning-rows))
  )
  thelist
)

; used to test is a variable's value is modified in a recursive call
(defun test-parameters (n)
  (format t "~a~%" n)
  (if (not (= n 0)) (test-parameters (- n 1)))
  (format t "~a~%" n)
)

; tests the add-to-list function, that it stays 
;  length <= 10 and sorted
(defun test-add ()
  (setf my-list '())
  (format t "~a~%" my-list)
  (setf my-list (add-to-list '(1 1) my-list 0))
  (format t "~a~%" my-list)
  (setf my-list (add-to-list '(1 2) my-list 1))
  (format t "~a~%" my-list)
  (setf my-list (add-to-list '(1 3) my-list 2))
  (format t "~a~%" my-list)
  (setf my-list (add-to-list '(1 4) my-list 3))
  (format t "~a~%" my-list)
  (setf my-list (add-to-list '(1 2) my-list 4))
  (format t "~a~%" my-list)
  (setf my-list (add-to-list '(1 5) my-list 5))
  (format t "~a~%" my-list)
  (setf my-list (add-to-list '(1 6) my-list 6))
  (format t "~a~%" my-list)
  (setf my-list (add-to-list '(1 7) my-list 7))
  (format t "~a~%" my-list)
  (setf my-list (add-to-list '(1 8) my-list 8))
  (format t "~a~%" my-list)
  (setf my-list (add-to-list '(1 9) my-list 9))
  (format t "~a~%" my-list)
  (setf my-list (add-to-list '(1 5) my-list 10))
  (format t "~a~%" my-list)
  (setf my-list (add-to-list '(1 10) my-list 10))
  (format t "~a~%" my-list)
)