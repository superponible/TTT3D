; algos.lsp

; contains several general processing algorithms used

; table is a list of lists 
;   the sublists are pairs (i j) such that:
;     i is an index value on the game board
;     j is the desirability of the move based on the heuristic
; the pair with the highest j value is returned
(defun max-list (table top)
  (setf max-val '(-1 -100000) len (length table) temptable table)
  (dotimes (n len max-val)
    (if (not (eq (caar temptable) 'x)) 
      (if (> (cadar temptable) (cadr max-val)) (setf max-val (list n (cadar temptable))))
    )
    (setf temptable (cdr temptable))
  )
;  (format t "~%~a~%~a~a~%" max-val table top)
  (if (eq top 1) (gather-list (cadr max-val) table) (list max-val))
)

; table is a list of lists 
;   the sublists are pairs (i j) such that:
;     i is an index value on the game board
;     j is the desirability of the move based on the heuristic
; the pair with the lowest j value is returned
(defun min-list (table top)
  (setf min-val '(-1 100000) len (length table) temptable table)
  (dotimes (n len min-val)
    (if (not (eq (caar temptable) 'x))
      (if (< (cadar temptable) (cadr min-val)) (setf min-val (list n (cadar temptable))))
    )
    (setf temptable (cdr temptable))
  )
  (if (eq top 1) (gather-list (cadr min-val) table) (list min-val))
)

; array is an array of lists, len is its size 
;   the sublists are pairs (i j) such that:
;     i is an index value on the game board
;     j is the desirability of the move based on the heuristic
; the pair with the highest j value is returned
(defun max-array (array len top)
  (setf max-val '(-1 -100000))
  (dotimes (n len max-val)
    (cond ((eq (aref array n) '-) ())
          ((> (aref array n) (cadr max-val)) (setf max-val (list n (aref array n))))
    )
  )
  (if (eq top 1) (gather-array (cadr max-val) array len) (list max-val))
)

; array is an array of lists, len is its size 
;   the sublists are pairs (i j) such that:
;     i is an index value on the game board
;     j is the desirability of the move based on the heuristic
; the pair with the lowest j value is returned
(defun min-array (array len top)
  (setf min-val '(-1 100000))
  (dotimes (n len min-val)
    (cond ((eq (aref array n) '-) ())
          ((< (aref array n) (cadr min-val)) (setf min-val (list n (aref array n))))
    )
  )
  (if (eq top 1) (gather-array (cadr min-val) array len) (list min-val))
)

; takes a list of (point value) pairs and uses val to return
;   a list of (point value) pairs where val=value for all pairs
(defun gather-list (val lst)
  (setf all-values '() i 0)
  (dolist (elt lst all-values)
    (cond ((eq (cadr elt) '-) ())
          ((eq (cadr elt) val) (setf all-values (append all-values (list (list i (cadr elt))))))
    )
    (setf i (+ i 1))
  )
)

; takes an array of values and uses val to return
;   a list of (point value) pairs where val=value and
;   index=point for all pairs
(defun gather-array (val array len)
  (setf all-values '())
  (dotimes (n len all-values)
    (if (eq (aref array n) val) (setf all-values (append all-values (list (list n (aref array n))))))
  )
)

; takes a list of (point value) pairs and selects one at random
(defun random-select-val (lst)
  (setf n (length lst))
  (do ((i 0) 
       (index (random n)))
      ((eq i index))
      (setf lst (cdr lst))
      (setf i (+ i 1))
  )
  (setf lst (car lst))
)

; adds the (point value) list to the move-list lst, keeping increasing
;    order by values
(defun add-to-list (elt lst size ply)
  ; drop first element in lst if size is 10 or more
  (if (>= size total-size)
    (setf lst (cdr lst))
  )

  ; initial case
  (if (= size 0) (return-from add-to-list (list elt)))
  ; add elt to lst in proper place
  (do ((n 0)
       (return-list '())
       (elt-value (cadr elt))
       (ltgt (if (oddp ply) #'>= #'<=))
      )
      ((>= n size) return-list)
    (if (null lst) (return-from add-to-list (append return-list (list elt))))
    (setf list-element (car lst))
    (cond ((funcall ltgt elt-value (cadr list-element))
             (setf return-list (append return-list (list list-element)))
          )
          (t (return-from add-to-list (append return-list (list elt) lst)))
    )
    (setf lst (cdr lst))
  )
)

; table is a list of lists 
;   the sublists are pairs (i j) such that:
;     i is an index value on the game board
;     j is the desirability of the move based on the heuristic
; the pair with the highest j value is returned
(defun max-list-greedy (lst top)
  (setf max-val '(-1 -100000) len (length lst) templst lst)
  (dotimes (n len max-val)
    (if (not (eq (caar templst) 'x)) 
      (if (> (cadar templst) (cadr max-val)) (setf max-val (list (caar templst) (cadar templst))))
    )
    (setf templst (cdr templst))
  )

  (if (eq top 1) (gather-list-greedy (cadr max-val) lst) (list max-val))
)

; table is a list of lists 
;   the sublists are pairs (i j) such that:
;     i is an index value on the game board
;     j is the desirability of the move based on the heuristic
; the pair with the lowest j value is returned
(defun min-list-greedy (lst top)
  (setf min-val '(-1 100000) len (length lst) templst lst)
  (dotimes (n len min-val)
    (if (not (eq (caar templst) 'x))
      (if (< (cadar templst) (cadr min-val)) (setf min-val (list (caar templst) (cadar templst))))
    )
    (setf templst (cdr templst))
  )

  (if (eq top 1) (gather-list-greedy (cadr min-val) lst) (list min-val))
)

; takes a list of (point value) pairs and uses val to return
;   a list of (point value) pairs where val=value
;   and point=point from lower level
(defun gather-list-greedy (val lst)
  (setf all-values '() i 0)
  (dolist (elt lst all-values)
    (cond ((eq (cadr elt) '-) ())
          ((eq (cadr elt) val) (setf all-values (append all-values (list (list (car elt) (cadr elt))))))
    )
    (setf i (+ i 1))
  )
)

; checks all possible rows to see if either side has a win
(defun check-for-win (board winning-rows)
  (dotimes (i 76 nil)
    (setf O-count 0)
    (setf X-count 0)
    (dotimes (j 4 nil)
      (setf coord (point-to-coord (aref winning-rows i j)))
      (cond ((eq (aref board (car coord) (cadr coord) (caddr coord)) 'X) (setf X-count (+ X-count 1)))
            ((eq (aref board (car coord) (cadr coord) (caddr coord)) 'O) (setf O-count (+ O-count 1)))
      )
    )
    (if (or (eq X-count 4) (eq O-count 4)) (return-from check-for-win t))
  )
)  

; returns opposite symbol
(defun other-side (side)
  (if (eq side 'X) 'O 'X)
)

; takes a point 0-63 and returns a list (0 0 0) - (3 3 3)
;   representing board indexes
(defun point-to-coord (point)
  (setf coord ())
  (setf coord 
    (do ()
        ((zerop point) coord)
        (setf coord (append (list (mod point 4)) coord))
        (setf point (floor point 4))
    )
  )
  (do ()
      ((= (length coord) 3) coord)
      (setf coord (append (list '0) coord))
  )
) 

; takes 3 coordinates and returns the point value 0-63
(defun coord-to-point (k j i)
  (setf point (+ (* k 16) (* j 4) (* i 1)))
)       
