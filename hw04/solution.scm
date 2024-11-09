;;;;;;;;;;;;;;;;;;; COMP 105 SCHEME ASSIGNMENT II;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Problem 2
;;;;  


;; (flip f) returns a function that has the opposite effect of function f

;; laws:
;;      ((flip f) x y) == (f y x)

(define flip (f)
        (lambda (x y) (f y x))
)

        ;; replace next line with good check-expect or check-assert tests
        (check-assert (not ((flip <) 3 4)))
        (check-assert (not ((flip <=) 3 4)))
        (check-assert ((flip =) 3 3))
        (check-expect ((flip append) '(a b c) '(1 2 3)) '(1 2 3 a b c))
        (check-expect ((flip cons) '() 1) '(1))
        



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Problem 3
;;;;  


;; (takewhile pred xs) Returns a list of elements of xs untill it reaches an 
;; element that doesn't satisfy pred

;; laws:
;;      (takewhile pred '())         == '()
;;      (takewhile pred (cons x xs)) == '(), when (not (pred x))
;;      (takewhile pred (cons x xs)) == (cons x (takewhile pred xs)), when 
;;                                                                    (pred x)

(define takewhile (pred xs)
        (if (null? xs)
                '()
                (if (pred (car xs))
                     (cons (car xs) (takewhile pred (cdr xs)))
                     '()   
                 )
         ) 
 )

        ;; replace next line with good check-expect or check-assert tests

        ;; (even? x) takes an integer input and returns whether or not its even
        ;; laws: 
        ;;      (even? x) == (= (mod x 2) 0)
        (define even? (x) (= (mod x 2) 0))
        
        (check-expect (takewhile even? '(2 4 6 7 8 10 12)) '(2 4 6))
        (check-expect (takewhile even? '(1 2 4 6 7 8 10 12)) '())


;; (dropwhile pred xs) 

;; laws:
;;      (dropwhile pred '())         == '()
;;      (dropwhile pred (cons x xs)) == xs, when (not (pred x))
;;      (dropwhile pred (cons x xs)) == (dropwhile pred xs), when (pred x)

(define dropwhile (pred xs)
        (if (null? xs)
                '()
                (if (pred (car xs))
                     (dropwhile pred (cdr xs))
                     xs
                 )
         ) 
 )

        ;; replace next line with good check-expect or check-assert tests
        (check-expect (dropwhile even? '(2 4 6 7 8 10 12)) '(7 8 10 12))
        (check-expect (dropwhile even? '(2 4 6 8 10 12)) '())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Problem 4
;;;;  


;; (ordered-by comp-f) takes a given comparitive function and returns a 
;;  predicate function that takes a list and tells whether that list is in the 
;; ordered of the given comparative function

;; laws: 
;;      ((ordered-by f) '())                  == #t
;;      ((ordered-by f) '(x))                 == #t
;;      ((ordered-by f) (cons x (cons y xs))) == #f when (not (f x y))
;;      ((ordered-by f) (cons x (cons y xs))) == ((ordered-by f) (cons y xs))
;;                                               when (f x y)


(define ordered-by (comp-f)
        (lambda (xs) 
                (if (null? xs)
                        #t
                        (if (null? (cdr xs))
                                #t
                                (if (comp-f (car xs) (cadr xs))
                                        ((ordered-by comp-f) (cons (cadr xs) 
                                                                (cddr xs)))
                                        #f
                                 )
                         )
                 )
         )
)

        ;; replace next line with good check-expect or check-assert tests
        (check-assert ((ordered-by <) '()))
        (check-assert ((ordered-by <) '(a)))
        (check-assert ((ordered-by <) '(1 2 3 6 7 98)))
        (check-assert (not ((ordered-by <) '(3 2 1))))
        (check-assert ((ordered-by =) '(3 3 3)))
        
        



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Problem 5
;;;;  

;; (max* xs) Returns the maximum of non empty list of integers xs

(define max* (xs)
        (foldr (lambda (x y) (if (> x y) x y)) 0 xs)
)

        ;; replace next line with good check-expect or check-assert tests
        (check-expect (max* '(1 2 34 4)) 34)
        (check-expect (max* '(1)) 1)
        

;; (sum xs) returns the sum of a nonempty list of integers

(define sum (xs)
        (foldr + 0 xs)
)

        ;; replace next line with good check-expect or check-assert tests\
        (check-expect (sum '(1)) 1)
        (check-expect (sum '(1 2 3 4 5)) 15)





;; (product xs) returns the product of a nonempty list of integers

(define product (xs)
        (foldr * 1 xs)
 )

        ;; replace next line with good check-expect or check-assert tests
        (check-expect (product '(0)) 0)
        (check-expect (product '(1 2 3 4 5)) 120)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Problem 6
;;;;  


;; (append xs ys) returns a list where ys has been added to the end of xs

(define append (xs ys) 
        (foldr cons ys xs)
 )

        ;; replace next line with good check-expect or check-assert tests
        (check-expect (append '(1 2 3) '(a b c)) '(1 2 3 a b c))
        (check-expect (append '(1 2 3) '()) '(1 2 3))


;; (reverse xs) 

;; laws:

(define reverse (xs)
        (foldl cons '() xs)
)

        ;; replace next line with good check-expect or check-assert tests
        (check-expect (reverse '(1 2 3)) '(3 2 1))
        (check-expect (reverse '()) '())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Problem 7
;;;;  


;; (map f xs) 

;; laws:

(define map (f xs)
        (foldr (lambda (x ys) (cons (f x) ys)) '() xs)
)

        ;; replace next line with good check-expect or check-assert tests
        (check-expect (map even? '(1 2 3)) '(#f #t #f))
        (check-expect (map even? '()) '())



;; (filter f xs) 

;; laws:

(define filter (f xs)
        (foldr (lambda (x ys) (if (f x) (cons x ys) ys)) '() xs)
)

        ;; replace next line with good check-expect or check-assert tests
        (check-expect (filter even? '(1 2 3)) '(2))
        (check-expect (filter even? '()) '())


;; (exists? f xs) 

;; laws:

(define exists? (f xs)
        (foldr (lambda (x ys) (if (f x) #t ys)) #f xs)
)

        ;; replace next line with good check-expect or check-assert tests
        (check-expect (exists? even? '(1 2 3)) '#t)
        (check-expect (exists? even? '()) '#f)



;; (filter f xs) 

;; laws:

(define all? (f xs)
        (foldr (lambda (x ys) (if (f x) ys #f)) #t xs)
)

        ;; replace next line with good check-expect or check-assert tests
        (check-expect (all? even? '(1 2 3)) '#f)
        (check-expect (all? even? '(4 2 6 10)) '#t)
        (check-expect (all? even? '()) '#t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Problem 8
;;;; 
(define member? (x s) (s x))

;; (evens x) 

;; laws:

(define evens (x)
        (if (number? x)
                (even? x)
                #f
         )
)

        ;; replace next line with good check-expect or check-assert tests
        (check-assert (member? 2 evens))
        (check-assert (member? 10 evens))
        (check-assert (not (member? 5 evens)))
        (check-assert (not (member? #t evens)))





;; (two-digits x) 

;; laws:

(define two-digits (x)
        (if (number? x)
                (if (< x 100)
                        (if (> x 9) 
                                #t 
                                #f
                         ) 
                 #f)
                #f      
        )
)

        ;; replace next line with good check-expect or check-assert tests
        (check-assert (not (member? 9 two-digits)))
        (check-assert (member? 10 two-digits))
        (check-assert (member? 99 two-digits))
        (check-assert (not (member? 100 two-digits)))




;; (add-element s x) 

;; laws:
;; (member? x (add-element x s)) == #t
;; (member? x (add-element y s)) == (s y), where (not (equal? y x))

(define add-element (s x)
        (lambda (y) (if (equal? y x) #t (s y)))
)

        ;; replace next line with good check-expect or check-assert tests
        (check-assert (member? 1 (add-element two-digits 1)))
        (check-assert (not (member? 2 (add-element two-digits 1))))





;; (add-element s1 s2) 

;; laws:
;; (member? x (union s1 s2))     == (or (s1 y) (s2 y))
(define union (s1 s2)
        (lambda (y) (or (s1 y) (s2 y)))
)

        ;; replace next line with good check-expect or check-assert tests
        (check-assert (member? 2 (union two-digits evens)))
        (check-assert (member? 95 (union two-digits evens)))
        (check-assert (not (member? 501 (union two-digits evens))))
        

;; ( inter s1 s2) 

;; laws:
;; (member? x (inter s1 s2))     == (and (s1 y) (s2 y))
(define inter (s1 s2)
        (lambda (y) (and (s1 y) (s2 y)))
)

        ;; replace next line with good check-expect or check-assert tests
        (check-assert (not (member? 2 (inter two-digits evens))))
        (check-assert (not (member? 95 (inter two-digits evens))))
        (check-assert (member? 94 (inter two-digits evens)))
        (check-assert (not (member? 501 (inter two-digits evens))))





;; (diff s1 s2) 

;; laws:
;; (member? x (diff  s1 s2))     == (and (s1 y) (not (s2 y)))
(define diff (s1 s2)
        (lambda (y) (and (s1 y) (not (s2 y))))
)

        ;; replace next line with good check-expect or check-assert tests
        (check-assert (not (member? 2 (diff two-digits evens))))
        (check-assert (member? 95 (diff two-digits evens)))
        (check-assert (not (member? 501 (diff two-digits evens))))
        (check-assert (not (member? 96 (diff two-digits evens))))