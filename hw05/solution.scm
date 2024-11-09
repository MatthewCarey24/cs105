;;;;;;;;;;;;;;;;;;; COMP 105 SCHEME ASSIGNMENT III ;;;;;;;;;;;;;;;

;; (even? x) takes any expression input and returns whether or not its even
;; laws: 
;;      (even? xs) == #f, when (not (atom? xs))
;;      (even? x)  == (= (mod x 2) 0)
(define even? (x) 
    (if (atom? x)
        (if (symbol? x)
                #f
                (= (mod x 2) 0)
         )
         #f        
    )
 )
 


(record not [arg])
(record or  [args])
(record and [args])

;;
;; Problem 2
;;

;; (list-of? pred v) takes a predicate function and any arbitrary scheme value
;; and returns #t if v is a list of values that all satisfy pred, otherwise #f

;; Laws:
;;      (list-of? pred '())         == #t
;;      (list-of? pred f)           == #f, where (function? f)
;;      (list-of? pred a)           == #f, where (atom? a)
;;      (list-of? pred (cons a as)) == (all? pred (cons a as))

(define list-of? (pred v)
    (if (null? v)
        #t
        (if (atom? v)
            #f
            (if (function? v)
                #f 
                (and (pred (car v)) (list-of? pred (cdr v))) 
            )
        )
    )
)
                (check-assert      (list-of? even? '()))
                (check-assert (not (list-of? even? '#t)))
                (check-assert (not (list-of? even? 'a)))
                (check-assert (not (list-of? even? (cons 'COMP 105))))
                (check-assert (not (list-of? even? even?)))
                (check-assert (not (list-of? even? '(2 '(4 2 6 8)))))
                (check-assert (not (list-of? even? '(2 '()))))
                (check-assert (list-of? even? '(2 34 4)))


;;
;; Problem 3
;; 

;; (formula v) takes any arbitrary scheme value and returns whether or not it 
;; is a valid boolean expression

;; Laws:
;;   (formula? b)                       == #t, where b is a boolean variable
;;   (formula? 's)                      == #t, where 's is a symbol 
;;                                                      representing a boolean
;;   (formula? (make-not f))            == (formula? f)
;;   (formula (make-or (cons f fs)))    == (and (formula? f) (list-of? 
;;                                                                formula? fs))
;;   (formula (make-and (cons f fs)))   == (and (formula? f) (list-of? 
;;                                                                formula? fs))
;;   (formula? val)                     == #f, otherwise

(define formula? (v)
    (if (symbol? v)
        #t
        (if (not? v)
            (formula? (not-arg v))
            (if (or? v)
                (and (formula? (car (or-args v))) 
                    (list-of? formula? (cdr (or-args v))))
                (if (and? v)
                    (and (formula? (car (and-args v))) 
                       (list-of? formula? (cdr (and-args v))))
                    #f
                )
            )
        )
    )
)


                (check-assert (list-of? formula? '()))
                (check-assert (formula? (make-not 'a)))
                (check-assert (formula? (make-or '(a b))))
                (check-assert (formula? (make-and '(a b c))))
                (check-assert (formula? (make-or (list3 (make-not 'a) 'b 'c))))
                (check-assert (formula? (make-and 
                 (list3 (make-not 'a) 
                  (make-or (list3 (make-not 'a) 'b 'c)) 'd))))
                (check-assert (formula? 
                                (make-and (list3 (make-not 'x) 
                                 (make-or (list3 (make-not 'a) 'y 'b)) 'z))))
                (check-assert (not (formula? 1)))
                (check-assert (not (formula? formula?)))


;;
;; Problem 4
;;

;; (eval-formula f env) takes a formula and an association list of symbols to 
;; boolean literals (environment) and returns whether or not the formula is 
;; satisfied in the environment, given every symbol in f is bound by env

;; Laws:
;;     (eval-formula b env)                       == b, where b is a boolean
;;     (eval-formula 's env)                      == (find 's env), where 's 
;;                                                  is a symbol representing a
;;                                                  boolean
;;     (eval-formula (make-not f) env)            == (not (eval-formula f env))
;;     (eval-formula (make-or (cons f fs)) env)   == (or (eval-formula f env)
;;                                            (eval-formula (make-or fs) env))
;;     (eval-formula (make-or '()) env)           == #f
;;     (eval-formula (make-and (cons f fs)) env)  == (and (eval-formula f env)
;;                                           (eval-formula (make-and fs) env))
;;     (eval-formula (make-and '()) env)          == #t

;; Notes: 'and' recursion base case is true, and 'or' recursion base case is 
;;       false ('neutral' case for each).

(define eval-formula (f env)
    (if (symbol? f)
        (find f env)
        (if (not? f)
            (not (eval-formula (not-arg f) env))
            (if (or? f)
                (if (null? (or-args f)) 
                    #f
                    (or (eval-formula (car (or-args f)) env) 
                        (eval-formula (make-or (cdr (or-args f))) env))
                )
                (if (and? f)
                    (if (null? (and-args f))
                        #t
                        (and (eval-formula (car (and-args f)) env) 
                            (eval-formula (make-and (cdr (and-args f))) env))
                    )
                    #f      
                )
            )
        )
    )
)

(val env (list2 (make-alist-pair 'a '#t) (make-alist-pair 'b '#f)))
(val env2 (list2 (make-alist-pair 'a '#f) (make-alist-pair 'b '#t)))
(val env3 (list3 (make-alist-pair 'a '#f) (make-alist-pair 'b '#t) 
                  (make-alist-pair 'c '#t)))

(check-assert (eval-formula 'a env))
(check-assert (not (eval-formula 'b env)))
(check-assert (not (eval-formula (make-not 'a) env)))
(check-assert (eval-formula (make-not 'b) env))
(check-assert (eval-formula (make-or '(a b)) env))
(check-assert (eval-formula (make-or '(a b)) env2))
(check-assert (not (eval-formula (make-and '(a b)) env)))
(check-assert (not (eval-formula (make-and '(a b)) env2)))
(check-assert (eval-formula (make-and '(b c)) env3))
(check-assert (eval-formula (make-and '((make-or '(a b)) c)) env3))




;;
;; Problem 5
;;

;; (solve-sat f fail succ) takes a boolean expression, and if there exists a 
;; environment that satisfies it, it calls succ with that environment and a 
;; resume continuation. If an environment doesn't exist, it calls fail.

(define solve-sat (f fail succ)
  ;; (solve-formula f bool cur fail succeed) attempts to extend environment 
  ;; 'cur' such that formula 'f' evaluates to boolean literal 'bool'. If it 
  ;; succeeds, it calls the 'success' continuation with the extended 
  ;; environment and fail, otherwise it calls 'fail'.

  ;; (solve-formula x             bool cur fail succeed) == 
  ;; (solve-symbol x bool cur fail succeed), where x is a symbol
  ;; (solve-formula (make-not f)  bool cur fail succeed) == 
  ;; (solve-formula f (not bool) cur fail succeed)
  ;; (solve-formula (make-or  fs) #t   cur fail succeed) == 
  ;; (solve-any fs #t cur fail succeed)
  ;; (solve-formula (make-or  fs) #f   cur fail succeed) == 
  ;; (solve-all fs #f cur fail succeed)
  ;; (solve-formula (make-and fs) #t   cur fail succeed) == 
  ;; (solve-all fs #t cur fail succeed)
  ;; (solve-formula (make-and fs) #f   cur fail succeed) == 
  ;; (solve-any fs #f cur fail succeed)
  (letrec 
    ([solve-formula
      (lambda (f bool cur fail succeed)
        (if (symbol? f)
            (solve-symbol f bool cur fail succeed)
            (if (not? f)
                (solve-formula (not-arg f) (not bool) cur fail succeed)
                (if (or? f)
                    (if bool
                        (solve-any (or-args f) #t cur fail succeed)
                        (solve-all (or-args f) #f cur fail succeed)
                    )
                    (if (and? f)
                        (if bool
                            (solve-all (and-args f) #t cur fail succeed)
                            (solve-any (and-args f) #f cur fail succeed)
                        )
                        (fail)
                    )
                )
            )
        )
      )
    ]
    ;; (solve-all fs bool cur fail succeed) extends the environment 'cur' such
    ;; that every formula in 'fs' evaluates to boolean literal 'bool'. If it
    ;; succeeds, it calls the 'success' continuation with the extended 
    ;; environment and the 'fail' continuation, otherwise it calls 'fail'.

    ;; (solve-all '()         bool cur fail succeed) == (succeed cur fail)
    ;; (solve-all (cons f fs) bool cur fail succeed) == 
    ;; (solve-formula f bool cur fail 
    ;;    (lambda (cur') (solve-all (cdr fs) bool cur' fail succeed)))
    [solve-all
      (lambda (fs bool cur fail succeed)
        (if (null? fs)
            (succeed cur fail)
            (solve-formula (car fs) bool cur fail 
              (lambda (cur' fail') 
                (solve-all (cdr fs) bool cur' fail' succeed)))
        )
      )
    ]
    ;; (solve-any fs bool cur fail succeed) takes a list of formulas, a desired
    ;; bool value, an environment, and fail and succeed continuations. It 
    ;; attempts to extend cur such that one formula in fs is bound to bool.

    ;; (solve-any '()         bool cur fail succeed) == (fail)
    ;; (solve-any (cons f fs) bool cur fail succeed) == 
    ;; (solve-formula f bool cur 
    ;;    (lambda () (solve-any fs bool cur fail succeed)) succeed)
    [solve-any
      (lambda (fs bool cur fail succeed)
        (if (null? fs)
            (fail)
            (solve-formula (car fs) bool cur
              (lambda () 
                (solve-any (cdr fs) bool cur fail succeed)) 
              succeed)
        )
      )
    ]
    ;; (solve-symbol x bool cur fail succeed) takes a symbol to evaluate, a 
    ;; boolean literal, a current environment, and fail and success 
    ;; continuations. It calls fail if x is bound to (not bool) in cur, calls 
    ;; succeed with current environment and the fail continuation if x is bound
    ;; to bool, otherwise it succeeds and extends cur to contain x mapped to 
    ;; bool.  

    ;; (solve-symbol x bool cur fail succeed) == 
    ;; (succeed (append (make-alist-pair x bool) cur) fail), where x is not 
    ;; bound in cur
    ;; (solve-symbol x bool cur fail succeed) == 
    ;; (succeed cur fail), where x is bool in cur
    ;; (solve-symbol x bool cur fail succeed) == 
    ;; (fail), where x is (not bool) in cur
    [solve-symbol
      (lambda (x bool cur fail succeed)
        (if (null? (find x cur))
            (succeed (bind x bool cur) fail)
            (if (= (find x cur) bool)
                (succeed cur fail)
                (fail)
            )
        )
      )
    ]
  )
  (solve-formula f #t '() fail succ)
))

;; Unit Tests:
(check-assert (function? solve-sat))            ; correct name
(check-error  (solve-sat))                      ; not 0 arguments
(check-error  (solve-sat 'x))                   ; not 1 argument
(check-error  (solve-sat 'x (lambda () 'fail))) ; not 2 args
(check-error  (solve-sat 'x (lambda () 'fail) 
                         (lambda (c r) 'succeed) 'z)) ; not 4 args

(check-error (solve-sat 'x (lambda () 'fail) 
                         (lambda () 'succeed))) ; success 
                                             ; continuation expects 
                                             ; 2 arguments, not 0
(check-error (solve-sat 'x (lambda () 'fail) 
                         (lambda (_) 'succeed))) ; success 
                                             ; continuation expects 
                                             ; 2 arguments, not 1

(check-error (solve-sat   ; failure continuation expects 0 arguments, not 1
                   (make-and (list2 'x (make-not 'x)))
                   (lambda (_) 'fail)
                   (lambda (_) 'succeed)))

(check-expect   ; x can be solved
  (solve-sat 'x (lambda () 'fail)
                (lambda (cur resume) 'succeed))
  'succeed)
(check-expect   ; x is solved by '((x #t))
  (solve-sat 'x (lambda () 'fail)
                (lambda (cur resume) (find 'x cur)))
                #t)
(check-expect   ; (make-not 'x) can be solved
  (solve-sat (make-not 'x)
             (lambda () 'fail)
             (lambda (cur resume) 'succeed))
             'succeed)
(check-expect   ; (make-not 'x) is solved by '((x #f))
  (solve-sat (make-not 'x)
             (lambda () 'fail)
             (lambda (cur resume) (find 'x cur)))
             #f)
(check-expect   ; (make-and (list2 'x (make-not 'x))) cannot be solved
  (solve-sat (make-and (list2 'x (make-not 'x)))
             (lambda () 'fail)
             (lambda (cur resume) 'succeed))
             'fail)
