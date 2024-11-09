;;;;;;;;;;;;;;;;;;; COMP 105 SCHEME ASSIGNMENT ;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 2


;; (contig-sublist? xs ys) returns if list xs exists as a continuous 
;; subsequence of list ys, otherwise it returns false

;; laws (if you want to attempt them; they are optional for this problem):
;;   (contig-sublist? ...) == ...
;;   ...
;; [optional notes about where laws come from, or difficulty, if any]

(define contig-sublist? (xs ys)
        (if (and (null? ys) (not (null? xs)))
            #f
            (if (null? xs) 
                #t 
                (if (and (not (null? (cdr xs))) (not (null? (cdr ys))))
                    (if (= (cadr xs) (cadr ys))
                        (contig-sublist? (cdr xs) (cdr ys))
                        (if (= (car xs) (car ys))
                            #f
                            (contig-sublist? xs (cdr ys))
                         )
                     )
                    (if (= (cdr xs) (cdr ys))
                        (contig-sublist? (cdr xs) (cdr ys))
                        (if (= (car xs) (car ys))
                            #f
                            (contig-sublist? xs (cdr ys))
                         )
                     )
                 )
             )
         )
 )

        ;; replace next line with good check-expect or check-assert tests
        (check-assert (contig-sublist? '() '()))
        (check-assert (contig-sublist? '() '(1 2 3)))
        (check-assert (not (contig-sublist? '(1 2 3) '())))
        (check-assert (contig-sublist? '(2 3 4) '(1 2 3 4)))
        (check-assert (not (contig-sublist? '(2 3 4) '(1 2 4 5))))
        (check-assert (not (contig-sublist? '(2 3 4) '(1 2 8 3 4 5))))
        



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 3


;; (flatten xs) takes a list of S-expressions and removes all internal 
;; parenthesis

;; laws:
;;   (flatten '())           == '()
;;   (flatten (cons '() xs)) == (flatten xs)
;;   (flatten (cons x xs))   == (cons x (flatten xs))
;;   (flatten (cons ys xs))  == (cons (flatten ys) (flatten xs))
;; [optional notes about where laws come from, or difficulty, if any]

(define flatten (xs)
    (if (= xs '())
        '()
        (if (= '() (car xs))
            (flatten (cdr xs))
            (if (atom? (car xs))
                (cons (car xs) (flatten (cdr xs)))
                (append (flatten (car xs)) (flatten (cdr xs)))
             )
         )
     )
 )

        ;; replace next line with good check-expect or check-assert tests
        (check-expect (flatten '((I Ching) (U Thant) (E Coli))) 
            '(I Ching U Thant E Coli))
        (check-expect (flatten '(((((a)))))) '(a))
        (check-expect (flatten '()) '())
        (check-expect (flatten '((a) () ((b c) d e))) '(a b c d e))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 4


;; (take n xs) returns the first n elements of list xs

;; laws:
;;   (take n '()) == '()
;;   (take 0 (cons x xs)) == '()
;;   (take n (cons x xs)) == (cons x (take (- n 1) xs))
;;   ...
;; [optional notes about where laws come from, or difficulty, if any]

(define take (n xs)
    (if (null? xs)
        '()
        (if (= n 0)
            '()
            (cons (car xs) (take (- n 1) (cdr xs)))
         )
     )
 )

        (check-expect (take 3 '()) '())
        (check-expect (take 0 '(1 2 3)) '())
        (check-expect (take 2 '(1 2 3)) '(1 2))
        



;; (drop n xs) returns a list thats the result of the first n elements of xs 
;; being removed

;; laws:
;;   (drop n '()) == '()
;;   (drop 0 (cons x xs)) == (cons x xs)
;;   (drop n (cons x xs)) == (drop (- n 1) xs)
;;   ...
;; [optional notes about where laws come from, or difficulty, if any]

(define drop (n xs)
    (if (null? xs)
        '()
        (if (= n 0)
            xs
            (drop (- n 1) (cdr xs))
         )
     )
 )

        ;; replace next line with good check-expect or check-assert tests
        (check-expect (drop 3 '()) '())
        (check-expect (drop 0 '(1 2 3)) '(1 2 3))
        (check-expect (drop 2 '(1 2 3)) '(3))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 5


;; (zip xs ys) converts a pair of list to a list of pairs, where the nth pair 
;; in the resulting list contains the nth element of each of the original lists

;; laws:
;;   (zip '() '())                 == (cons '() '())
;;   (zip (cons x xs) (cons y ys)) == (cons (cons x y) (zip xs ys))

;; [optional notes about where laws come from, or difficulty, if any]

(define zip (xs ys)
    (if (null? xs)
        '()
        (if (null? ys)
            '()
            (cons (cons (car xs) (cons (car ys) '())) (zip (cdr xs) (cdr ys)))
         )
     )
 )

        (check-expect (zip '() '()) '())
        (check-expect (zip '(1 2 3) '(a b c)) '((1 a) (2 b) (3 c)))



;; (unzip ps) converts a list of pairs to a pair of lists, such that the first 
;; entry of each pair is in the first list, and the 2nd entry is in the 2nd 
;; list

;; laws (if you want to attempt them; they are optional for unzip):
;;   (unzip '())                   == '()
;;   (unzip (cons (cons x y) '())) == (cons (cons x '()) (cons y '()))
;;   (unzip (cons (cons x y) xs))  == (cons (cons x (car (unzip xs))) 
;;                                      (cons y (cdr (unzip xs))))

;; [optional notes about where laws come from, or difficulty, if any]

(define unzip (ps)
    (if (null? ps)
        '(() ())
        (let ((unzipped-rest (unzip (cdr ps))))
         (if (null? unzipped-rest)
             (cons (cons (car (car ps)) '()) (cons (cdr (car ps)) '()))
             (cons (cons (car (car ps)) (car unzipped-rest)) 
                (cons (flatten(cons (cdr (car ps)) (cdr unzipped-rest))) '()))
          )
         )
     )
 )

        (check-expect (unzip '()) '(() ()))
        (check-expect (unzip '((1 a))) '((1) (a)))
        (check-expect (unzip '((1 a) (2 b) (3 c))) '((1 2 3) (a b c)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;  Exercise 6


;; (arg-max f xs) returns the x in xs such that (f x) gives the largest value 
;; of any x in xs.

;; laws:
;;   (arg-max f (cons a '())) == (f a)
;;   (arg-max f (cons a as)) == (if (> (f a) (arg-max f as)) (f a) 
;;                                                        (arg-max f as))


(define arg-max (f xs)
    (if (null? (cdr xs))
        (car xs)
        (let ((rest-max (arg-max f (cdr xs))))
         (if (> (f (car xs)) (f rest-max))
            (car xs)
            rest-max
          )
         )
     ) 
 )

        ;; replace next line with good check-expect or check-assert tests
        (check-expect (arg-max car '((105 PL) (160 Algorithms) (170 Theory))) 
                                                                '(170 Theory))
        (define square (a) (* a a))
        (check-expect (arg-max square '(5 4 3 2 1)) 5)
