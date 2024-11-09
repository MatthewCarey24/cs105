;;  Author: Richard Townsend
;;  Date: 3-4-22
;;
;;  This example demonstrates how to craft a polymorphic, recursive function
;;  in Typed uScheme. Here are the possible forms for defining and naming a
;;  function, and why we can't use them in an obvious way for this purpose:
;;
;;  (val name exp) -- exp can't be recursive
;;  (val-rec [name : tyexp] exp) -- exp can be recursive, but must be a lambda.
;;                                  We need to start with type-lambda so we can 
;;                                  specify polymorphic parameters.
;;  (define tyex name (formals) exp) -- Just syntactic sugar for val-rec, so
;;                                      same issue.
;;
;;  So how can we write a polymorphic, recursive function? We start with a
;;  type-lambda (to introduce type variables), and follow with a letrec defining
;;  a recursive function that uses those type variables. In other words, the
;;  letrec defines the recursive function that does all the work (and adheres to
;;  our laws). We then just return the locally defined recursive function!



;; my_length : (forall ['a] ((list 'a) -> int))
;;
;; Given a list of any type xs, return the length of xs.
;;
;; Laws:
;;  (my_length '()) = 0
;;  (my_length (cons y ys)) = (+ 1 (my_length ys))
(val my_length
  (type-lambda ['a]
    (letrec [([length-mono : ((list 'a) -> int)]
                  (lambda ([xs : (list 'a)])
                    (if ([@ null? 'a] xs)
                        0
                        (+ 1 (length-mono ([@ cdr 'a] xs))))))]
            length-mono)))

(check-type my_length (forall ['a] ((list 'a) -> int)))

(check-expect ([@ my_length int] '(1 2 3)) 3)
(check-expect ([@ my_length bool] '(#t #f)) 2)
(check-expect ([@ my_length (list int)] '((1 2) (3))) 2)


;; Here's another example of a polymorphic, recursive function.
;;
;; reverse : (forall ['a] ((list 'a) -> (list 'a))
;;
;; Reverse a list xs of any type.
;;
;; Laws:
;;  (reverse '()) = '()
;;  (reverse (cons y ys)) = (append (reverse ys) (list1 y))

(val reverse
  (type-lambda ['a]
    (letrec [([reverse-mono : ((list 'a) -> (list 'a))]
                 (lambda ([xs : (list 'a)])
                    (if ([@ null? 'a] xs)
                        [@ '() 'a]
                        ([@ append 'a] (reverse-mono ([@ cdr 'a] xs))
                                       ([@ list1 'a] ([@ car 'a] xs))))))]
            reverse-mono)))

(check-type reverse (forall ['a] ((list 'a) -> (list 'a))))

(check-expect ([@ reverse int] '(1 2 3)) '(3 2 1))
(check-expect ([@ reverse bool] '(#t #f)) '(#f #t))
(check-expect ([@ reverse (list int)] '((1 2) (3))) '((3) (1 2)))
