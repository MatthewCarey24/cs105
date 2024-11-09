;;;;;;;;;;;;;;;;;;; COMP 105 SCHEME ASSIGNMENT ;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PART B
;;

;; (drop n xs) returns a list thats the result of the first n elements of xs 
;; being removed

;; laws:
;;   (drop n '())               == '()
;;   (drop 0 (cons x xs))       == (cons x xs)
;;   (drop (+ n 1) (cons x xs)) == (drop n xs)
(val drop
  (type-lambda ['a]
    (letrec [([drop-mono : (int (list 'a) -> (list 'a))]
                  (lambda ([n : int] [xs : (list 'a)])
                    (if ([@ null? 'a] xs)
                        (@ '() 'a)
                        (if ((@ = int) n 0)
                                xs
                                (drop-mono (- n 1) ((@ cdr 'a) xs))))))]
            drop-mono)))


(check-type drop (forall ['a] (int (list 'a) -> (list 'a))))

(check-expect ([@ drop int] 2 '(1 2 3)) '(3))
(check-expect ([@ drop bool] 1 '(#t #f #t)) '(#f #t))
(check-expect ([@ drop (list int)] 1 '((1 2) (3))) '((3)))
(check-expect ([@ drop int] 5 '(1 2 3)) (@ '() int))
(check-expect ([@ drop int] 3 (@ '() int)) (@ '() int))



;; (takewhile pred xs) Returns a list of elements of xs untill it reaches an 
;; element that doesn't satisfy pred

;; laws:
;;      (takewhile pred '())         == '()
;;      (takewhile pred (cons x xs)) == '(), when (not (pred x))
;;      (takewhile pred (cons x xs)) == (cons x (takewhile pred xs)), when 
;;                                                                    (pred x)
(val takewhile
  (type-lambda ['a]
    (letrec [([take-mono : (('a -> bool) (list 'a) -> (list 'a))]
      (lambda ([f : ('a -> bool)] [xs : (list 'a)])
        (if ([@ null? 'a] xs)
            [@ '() 'a]
            (if (f ([@ car 'a] xs))
                    ((@ cons 'a) ([@ car 'a] xs) (take-mono f ([@ cdr 'a] xs)))
                    [@ '() 'a]))))]
            take-mono)))

(check-type takewhile (forall ['a] (('a -> bool) (list 'a) -> (list 'a))))

(check-expect ([@ takewhile int] (lambda ([n : int]) ([@ = int] (mod n 2) 0)) 
                [@ '() int]) [@ '() int])
(check-expect ([@ takewhile int] (lambda ([n : int]) ([@ = int] (mod n 2) 0)) 
                '(1 2 4 2)) [@ '() int])
(check-expect ([@ takewhile int] (lambda ([x : int]) ([@ = int] (mod x 2) 0)) 
                '(1 3 5 7 9 11)) [@ '() int]) 
(check-expect ([@ takewhile int] (lambda ([x : int]) ([@ = int] (mod x 2) 0)) 
                '(2 4 6 7 8 10 12)) '(2 4 6))
(check-expect ([@ takewhile int] (lambda ([x : int]) ([@ = int] (mod x 2) 0)) 
                '(2)) '(2))
(check-expect ([@ takewhile int] (lambda ([x : int]) ([@ = int] (mod x 2) 0)) 
                '(246 8)) '(246 8))
