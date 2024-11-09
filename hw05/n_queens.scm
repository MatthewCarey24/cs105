;;
;; Question 1 - Overall continuations (like base cases)
;;
(val fail-queens (lambda () 'no-solution)) ;function because thats how they get passed in the continuation
(val succ-queens (lambda (placement resume) 'N-queens-success)) ; Dont use parameters, just supposed to return overall success, but succ funcs always take curr state and resume func

;;
;; Question 2
;;
;; a) it allows us to write laws discriminating based on the values of 
;; left-to-place and safe
;;
;; b) We pass N fail and succ as parameters to a lambda as left-to-place, f and
;;  s because it stores their current value which we can use when we come back
;;  as we move through; globals (N, fail, succ) are constantly changing, want 
;;  to store what they were when we were at this point

;;
;; Question 3
;;
;; a) left-to-place = 0
;; b) safe          = '()
;; c) left-to-place = (+ m 1)  (how to show that a natural is not 0)
;;    safe          = (cons sq sqs)

;;
;; Question 4
;;
;; (prune-squares sq safe) takes a square sq where the current queen
;; is being placed and a list of currently safe squares safe and returns
;; the list of squares in safe that are still safe after a queen is placed
;; on sq.

;; (place-queens placed 0 safe f s)                == (s placed f)
;; (place-queens placed (+ m 1) '() f s)           == (f)
;; (place-queens placed (+ m 1) (cons sq sqs) f s) == (place-queens (cons sq placed) 
;;                                                                  m 
;;                                                                  (prune-squares sq sqs) 
;;                                                                  (lambda () (place-queens placed (+ m 1) sqs f s))
;;                                                                   s)


(define N-queens (N fail succ)
  (letrec
    ([place-queens
      (lambda (placed left-to-place safe f s)
        (if (= left-to-place 0)
                (s placed f)
                (if (null? safe)
                        f
                        (place-queens (cons (car safe) placed) (- left-to-place 1) (prune-squares (car safe) safe) (lambda () (place-queens placed left-to-place (cdr safe) f s)) s)
                 )
         )
      )])

(place-queens '() N (empty-board N) fail succ)))



;; Appendix:
(define empty-board (n)
(letrec
[(empty-board-with-curr
(lambda (x y)
(if (&& (<= n x) (<= n y))
(cons (make-square x y) '())
(if (<= n y)
(cons (make-square x y)
(empty-board-with-curr (+ 1 x) 1))
(cons (make-square x y)
(empty-board-with-curr x (+ 1 y)))))))]
(empty-board-with-curr 1 1)))
(define prune-squares (q safe)
(letrec (
[same-col? (lambda (s s') (= (square-col s) (square-col s')))]
[same-row? (lambda (s s') (= (square-row s) (square-row s')))]
[abs (lambda (x) (if (< x 0) (* -1 x) x))]
[same-dia? (lambda (s s') (= (abs (- (square-col s) (square-col s')))
(abs (- (square-row s) (square-row s')))))]
; recursive helper function so we don't redefined the above
; auxiliary functions every iteration
[prune-squares'
(lambda (safe')
(if (null? safe')
'()
(let ([s (car safe')]
[ss (cdr safe')])
(if (|| (same-col? q s)
(|| (same-row? q s)
(same-dia? q s)))
(prune-squares q ss)
(cons s (prune-squares q ss))))))])
(prune-squares' safe)))
