;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; step 5
;;

(check-type 3 int)
(check-type #t bool)
(check-type 'hello sym)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; step 6
;;

(check-type (if #t 1 2) int)
(check-type (if #t #f #t) bool)
(check-type-error (if 'hi 1 2))
(check-type-error (if 2 1 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; step 7
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; step 9
;;

(val x 5)
(val y #t)
(val name 'hello)
(check-type x int)
(check-type y bool)
(check-type name sym)
(check-type-error (if name 1 2))
(check-type-error (if x 1 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; step 10
;;

(check-type (+ 12 1) int)
; (check-type (or #f #t) bool)
(check-type-error (+ #f 1))
; (check-type-error (or #f 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; step 11
;;

(check-type (let ((x 3)) x) int)           
(check-type (let ((x #t)) x) bool)         
(check-type (let ((name 'hello)) name) sym)
(check-type (let ((x 1) (y 2)) (+ x y)) int)    
; (check-type-error (let ([x 1] [y]) y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; step 12
;;

(check-type (lambda ([x : int]) x) (int -> int))             
(check-type (lambda ([x : int]) (+ x 1)) (int -> int))       
(check-type (lambda ([x : int] [y : int]) (+ x y)) (int int -> int))   
(check-type (lambda ([x : bool]) (if x #t #f)) (bool -> bool))
(check-type (lambda ([x : bool]) (if x 1 4)) (bool -> int))
(check-type (lambda ([x : bool]) (if x 'hi 'hello)) (bool -> sym))
(check-type (lambda ([x : (int -> bool)]) (if (x 1) 2 3)) 
                                                        ((int -> bool) -> int))

(check-type-error (lambda ([x : matt]) (+ x #t)))
(check-type-error (lambda ([x : int]) (+ x #t)))
(check-type-error (lambda ([x : bool]) (+ x 2)))
(check-type-error (lambda ([x : int]) (if x 1 2)))     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; step 13
;;
; set
(check-type (set x 3) int)
(check-type (set y #f) bool)
(check-type (set name 'matt) sym)

(check-type-error (set x #f))
(check-type-error (set unbound 2))

; while
(check-type (while #f (+ 1 2)) unit)

(check-type-error (while 2 (- 3 2)))

; begin
(check-type (begin #t 10 (+ 2 3)) int)
(check-type (begin #f) bool)
(check-type-error (begin #t 10 (+ #f 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; step 14
;;
(check-type (let* ((x 1) (y (+ x 2))) y) int)
(check-type (let* ((x 3) (y (* x 2))) (+ x y)) int)
(check-type (let* ((x #t) (y (if x 1 2))) y) int)
(check-type (let* ((a 'hello) (b a)) b) sym)

(check-type-error (let* ((x 1) (y #t)) (+ x y)))
(check-type-error (let* ((x 'hi) (y (+ x 1))) y))
(check-type-error (let* ((x 2) (y x)) (if y 1 'string)))
(check-type-error (let* () (display "No bindings")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; step 15
;;
(check-type (letrec [([length-mono : ((list int) -> int)] 
(lambda ([xs : (list int)])
(if ([@ null? int] xs)
0
(+ 1 (length-mono ([@ cdr int] xs))))))]
length-mono) ((list int) -> int))

(check-type-error (letrec [([length-mono : ((list bool) -> int)] 
(lambda ([xs : (list int)])
(if ([@ null? int] xs)
0
(+ 1 (length-mono ([@ cdr int] xs))))))]
length-mono))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; step 16
;;
(val-rec [t1 : (int -> int)] (lambda ([x : int]) (* x 1)))
(define int t2 ([x : int]) (* 2 x))

(check-type (t1 3) int)
(check-type-error (t1 #t))
(check-type (t2 5) int)
(check-type (t2 0) int)
(check-type-error (t2 #t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; step 17
;;
(check-type (type-lambda ['a]
    (letrec [([drop-mono : (int (list 'a) -> (list 'a))]
                  (lambda ([n : int] [xs : (list 'a)])
                    (if ([@ null? 'a] xs)
                        (@ '() 'a)
                        (if ((@ = int) n 0)
                                xs
                                (drop-mono (- n 1) ((@ cdr 'a) xs))))))]
            drop-mono)) (forall ['a] (int (list 'a) -> (list 'a))))

(check-type-error (type-lambda ['a]
    (letrec [([drop-mono : (int (list 'a) -> (list 'a))]
                  (lambda ([n : bool] [xs : (list 'a)])
                    (if ([@ null? 'a] xs)
                        (@ '() 'a)
                        (if ((@ = int) n 0)
                                xs
                                (drop-mono (- n 1) ((@ cdr 'a) xs))))))]
            drop-mono)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; step 18
;;
(check-type '() (forall ['a] (list 'a)))
(check-type ((@ cons int) 1 (@ '() int)) (list int))
(check-type ((@ cons int) 1 ((@ cons int) 2 (@ '() int))) (list int))
(check-type-error '(1, 2, #f))