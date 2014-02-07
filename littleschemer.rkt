#lang racket

(require rackunit)

(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

                        ;; 1. Toys
;; The Law of Car
;; The primite car is defined only for non-empty lists.

(check-equal? (car (list 1 2 3 4)) 1)
(check-equal? (car (list "a" "b" "c")) "a")
(check-equal? (car (list (list 1) 1 2 3)) (list 1))

;; The law of Cdr
;; the primitive cdr is definedonly for non-empty lists.
;; the cdr of any non-empty list is always another list.

(check-equal? (cdr (list 1 2)) (list 2))
(check-equal? (cdr (list 1 2 3)) (list 2 3))

;;The law of Cons
;; The primitieve cons takes two argumetns.
;; the second argumetns to cons bust be a a list.
;; the result is a list.

(check-equal? (cons 1 '()) (list 1))
(check-equal? (cons 1 (list 2)) (list 1 2))

;; The law of Null?
;; the primitive null? is defined only for lists.

(check-equal? (null? '()) true)
(check-equal? (null? (list 1)) false)
(check-equal? (null? (list (list ))) false)

;; The Law of Eq?
;; The primitive eq? takes two arguments. Each be be
; a non-numeric atom.

(check-equal? (eq? "a" "a" ) true)
(check-equal? (eq? "a" "aa" ) false)
                                
                    ;; 2. Do it, do it again, and again, and again.
(define (lat? l) 
  (cond [(null? l) true]
        [(atom? (car l)) (lat? (cdr l))]
        [else false]))

(check-equal? (lat? '()) true)
(check-equal? (lat? (list 1)) true)
(check-equal? (lat? (list 1 (list 2))) false)

(define (member? a l)
  (cond [(null? l) false]
        [(eq? a (car l)) true]
        [else (member? a (cdr l))]))

(check-equal? (member? 1 '()) false)
(check-equal? (member? 1 (list 1)) true)
(check-equal? (member? 1 (list 3 2 1)) true)

;; The First Commandment (preliminary)
;; Alwayas ask null? as the first question in expressing any function.

                        ;; 3. Cons the Magnificient.

(define (rember a lat)
  (cond [(null? lat) '()]
        [(eq? a (car lat)) (cdr lat)]
        [else (cons (car lat) (rember a (cdr lat)))]))


(check-equal? (rember 1 '()) '())
(check-equal? (rember 1 (list 1)) '())
(check-equal? (rember 1 (list 1 2)) (list 2))

;; The Second Commandment
;; Use cons to buid lists.

(define (firsts l)
  (cond [(null? l) '()]
        [else (cons (car (car l)) (firsts (cdr l)))]))

(check-equal? (firsts '()) '())
(check-equal? (firsts (list (list 1))) (list 1))
(check-equal? (firsts (list (list 1 22) (list 2 3) (list 3 3))) (list 1 2 3))

;; The Third Commandment
;; When building a list, describe the first typical element, 
;; and then cons it on the the natural recursion.

(define (insertR n o lat)
  (cond [(null? lat) '()]
        [(eq? o (car lat)) (cons n (cons o ( cdr lat)))]
        [else (insertR (n o (cdr lat)))]))

(check-equal? (insertR 1 2 '()) '()) 
(check-equal? (insertR 1 2 (list 2 3)) (list 1 2 3))
(check-equal? (insertR "a" "b" (list "b" "c")) (list "a" "b" "c"))

(define (insertL n o lat)
  (cond [(null? lat) '()]
        [(eq? o (car lat)) (cons o (cons n (cdr lat)))]
        [else (cons (car lat) (insertL n o (cdr lat)))]))

(check-equal? (insertL "c" "b"  '()) '())
(check-equal? (insertL "c" "b" (list "a" "b")) (list "a" "b" "c"))

(define (subst n o lat)
  (cond [(null? lat) '()]
        [(eq? o (car lat)) (cons n (cdr lat))]
        [else (cons (car lat) (subst n o (cdr lat)))]))

(check-equal? (subst "new" "old" '()) '())
(check-equal? (subst "new" "old" (list "old" "hello")) (list "new" "hello")) 

(define (subst2 n o1 o2 lat)
  (cond [(null? lat) '()]
        [(or (eq? o1 (car lat))
             (eq? o2 (car lat))) (cons n (cdr lat))]
        [else (cons (car lat) (subst2 n o1 o2 (cdr lat)))]))

(check-equal? (subst2 "new" "old1" "old2" '()) '())
(check-equal? (subst2 "new" "old1" "old2" (list "old1" "old2")) (list "new" "old2"))
(check-equal? (subst2 "vanilla" "chocolate" "banana"
                      (list "banana" "ice" "cream" "with" "chocolate" "topping"))
              (list "vanilla" "ice" "cream" "with" "chocolate" "topping"))

(define (multirember a lat)
  (cond [(null? lat) '()]
        [(eq? a (car lat)) (multirember a (cdr lat))]
        [else (cons (car lat) (multirember a (cdr lat)))]))

(check-equal? (multirember "a" '()) '())
(check-equal? (multirember "a" (list "a")) '())
(check-equal? (multirember "a" (list "a" "b" "a" "c" "a"))
              (list "b" "c"))

(define (multiinsertR n o lat)
  (cond [(null? lat) '()]
        [(eq? o (car lat)) (cons o (cons n (multiinsertR n o (cdr lat))))]
        [else (cons (car lat) (multiinsertR n o (cdr lat)))]))

(check-equal? (multiinsertR "new" "old" '()) '())
(check-equal? (multiinsertR  "new" "old" (list "old" "old" "old"))
              (list "old" "new" "old" "new" "old" "new"))

(define (multiinsertL n o lat)
  (cond [(null? lat) '()]
        [(eq? o (car lat)) (cons n (cons o (multiinsertL n o (cdr lat))))]
        [else (cons (car lat) (multiinsertL n o (cdr lat)))]))

(check-equal? (multiinsertL "new" "old" '()) '())
(check-equal? (multiinsertL  "new" "old" (list "old" "old" "old"))
              (list "new" "old" "new" "old" "new" "old"))

;; The fourth Commandment (preliminary)
;; Always change at least one argument while recurring.
;; It must be changed to be closer to termination. The changing
;; argument must be tested in the termination condition:
;; when using cdr, test terminationwith null?.

(define (multisubst n o lat)
  (cond [(null? lat) '()]
        [(eq? o (car lat)) (cons n (multisubst n o (cdr lat)))]
        [else (cons (car lat) (multisubst n o (cdr lat)))]))

(check-equal? (multisubst "new" "old" '()) '())
(check-equal? (multisubst "new" "old" (list "old" 1 "old" 1))
              (list "new" 1 "new" 1))

                ;; Number Games

(define (my-add a b)
  (cond [(zero? b) a]
        [else (my-add (add1 a) (sub1 b))]))

(check-equal? (my-add 5 0) 5)
(check-equal? (my-add 0 0) 0)
(check-equal? (my-add 5 5) 10)
(check-equal? (my-add 5 6) 11)

(define (my-minus a b)
  (cond [(zero? b) a]
        [else (sub1 (my-minus a (sub1 b)))]))

(check-equal? (my-minus 0 0) 0)
(check-equal? (my-minus 5 0) 5)
(check-equal? (my-minus 5 5) 0)
(check-equal? (my-minus 10 5) 5)

;; The First Commandment (first revision)
;; When recurring ona list of atoms, lat, ask two questions
;; about it: (null? lat) and else
;; when recurring on a number, n, ask two questions
;; about it: (zero? n) and else.

(define (addtup tup)
  (cond [(null? tup) 0]
        [else (my-add (car tup) (addtup (cdr tup)))]))

(check-equal? (addtup '()) 0)
(check-equal? (addtup (list 1)) 1)
(check-equal? (addtup (list 1 3)) 4)
(check-equal? (addtup (list 1 2 3)) 6)

;; The Fourth Commandment
;; Always change at least one argumetn while recurring. It
;; must be changed to be close to terminaton. The changing
;; argument must be tested in the termination condition:
;; when using cdr, test termination with null?
;; and when using sub1, test termination with zero?

(define (my-mult a b)
  (cond [(zero? b) 0]
        [else (my-add a (my-mult a (sub1 b)))]))

(check-equal? (my-mult 0 0) 0)
(check-equal? (my-mult 1 1) 1)
(check-equal? (my-mult 1 2) 2)
(check-equal? (my-mult 5 5) 25)
(check-equal? (my-mult 10 5) 50)

;; The Fifth Commandment
;; When building a value with +, always use 0 for the value of the
;; termination line, for adding 0 does not change the value of an
;; addition.

;; When building a value with x, always use 1 for the value of the
;; terminating line, for multiplying by 1 does not change the value
;; of a multiplication.

;; when building a value with cons, always consider () for the value
;; of the terminatin line.

(define (tup+ tup1 tup2)
  (cond [(and (null? tup1) (null? tup2)) '()]
        [else (cons (my-add (car tup1) (car tup2))
                    (tup+ (cdr tup1) (cdr tup2)))]))

(check-equal? (tup+ '() '()) '())
(check-equal? (tup+ (list 3 6 9 11 4)
                    (list 8 5 2 0 7)) (list 11 11 11 11 11))

(define (>^ a b)
  (cond [(zero? a) false] 
        [(zero? b) true]
        [else (>^ (sub1 a) (sub1 b))]))

(check-equal? (>^ 3 3) false)
(check-equal? (>^ 4 1) true)

(define (<^ a b)
  (cond [(zero? b) false]
        [(zero? a) true]
        [else (<^ (sub1 a) (sub1 b))]))

(check-equal? (<^ 0 0) false)
(check-equal? (<^ 1 3) true)
(check-equal? (<^ 4 1) false)

(define (=^ a b)
  (cond [(>^ a b) false]
        [(<^ a b) false]
        [else true]))

(check-equal? (=^ 0 0) true)
(check-equal? (=^ 1 1) true)
(check-equal? (=^ 2 3) false)
(check-equal? (=^ 12 0) false)

[define (pow^ a b)
  (cond [(zero? b) 1]
        [else (my-mult a (pow^ a (sub1 b)))])]

(check-equal? (pow^ 0 1) 0)
(check-equal? (pow^ 2 3) 8)
(check-equal? (pow^ 4 5) 1024)

(define (div^ a b)
  (cond [(<^ a b) 0]
        [else (add1 (div^ (my-minus a b) b))]))

(check-equal? (div^ 15 4) 3)

(define (length^ lat)
  (cond [(null? lat) 0]
        [else (add1 (length^ (cdr lat)))]))

(check-equal? (length^ '()) 0)
(check-equal? (length^ (list 'a)) 1)
(check-equal? (length^ (list 'a 'b)) 2)
(check-equal? (length^ (list 'a 'b 'c)) 3)

(define (pick n lat)
  (cond [(zero? (sub1 n)) (car lat)]
        [else (pick (sub1 n) (cdr lat))]))

(check-equal? (pick 4 (list 'lasagna 'spaghetti 'ravioli 'macaroni 'meatball)) 'macaroni)

(define (rempick n lat)
  (cond [(zero? (sub1 n)) (cdr lat)]
        [else (cons (car lat)
                    (rempick (sub1 n) (cdr lat)))]))

(check-equal? (rempick 3 (list 'hotdogs 'with 'hot 'mustard)) (list 'hotdogs 'with 'mustard) '(list 'hotdogs 'with 'mustard))

(define (no-nums lat)
  (cond [(null? lat) '()]
        [(number? (car lat)) (no-nums (cdr lat))]
        [else (cons (car lat) (no-nums (cdr lat)))]))

(check-equal? (no-nums (list 1 3 'x 'b 5 'z 'b))
              (list 'x 'b 'z 'b))

(define (all-nums lat)
  (cond [(null? lat) '()]
        [(number? (car lat)) (cons (car lat) (all-nums (cdr lat)))]
        [else (all-nums (cdr lat))]))

(check-equal? (all-nums (list 1 3 'x 'b 5 'z 'b))
              (list 1 3 5))

(define (occur a lat)
  (cond [(null? lat) 0]
        [(eq? a (car lat)) (add1 (occur a (cdr lat)))]
        [else (occur a (cdr lat))]))

(check-equal? (occur 'b (list 'x '2 'x '5 'x)) 0)
(check-equal? (occur 'x (list 'x '2 'x '5 'x)) 3)

(define (one? a)
  (= 1 a))

(define (rempick^ a lat)
  (cond [(null? lat) '()]
        [(one? a) (cdr lat)]
        [else (cons (car lat)(rempick^ (sub1 a) (cdr lat)))]))

(check-equal? (rempick^ 3 (list 'lemon 'meringue 'salty 'pie))
              (list 'lemon 'meringue 'pie))

                ;; 5. *Oh My Gawd*: Its Full Of Stars

(define (rember* a l)
  (cond [(null? l) '()]
        [(atom? (car l))
         (cond [(eq? (car l) a) (rember* a (cdr l))]
               [else (cons (car l) (rember* a (cdr l)))])]
        [else (cons (rember* a (car l))
                    (rember* a (cdr l)))]))

(check-equal? (rember* 'x '()) '())
(check-equal? (rember* 'x (list 'x (list 'x (list (list 'x)))))
              (list(list(list(list)))))

(define (insertR* new old l)
  (cond [(null? l) '()]
        [(atom? (car l))
         (cond [(eq? old (car l)) (cons old (cons new (cdr l)))]
               [else (cons (car l) (insertR* new old (cdr l)))])]
        [else (cons (insertR* new old (car l))
                    (insertR* new old (cdr l)))]))

(check-equal? (insertR* 'y 'x (list 'x 'z)) (list 'x 'y 'z))
(check-equal? (insertR* 'y 'x (list (list (list 'x 'z))))
              (list (list (list 'x 'y 'z))))

;; The First Commandment
;; final version
;; When recurring on a list of atoms, lat, ask two questions
;; about it: (null? lat) and else.
;; when recurring on a number, n, ask two questions about it:
;; (zero? n) and else.
;; when recurring on alist of S-expressios, l, ask three questions
;; about it: (null? l), (atom? (car l)), and else.

;; The Fourth Commandment
;; final version
;; Always change at least one argument while recurring.
;; When recurring on a list of atoms, lat, use (cdr lat). When
;; recurring on a number, n, use (sub1 n). And when reucurring
;; on a list of S-expresions, l, use (car l) and (cdr l) if neither
;; (null? l) nor (atom? (car l) are true.

;; it must be changed to be close to termination. The changing
;; argument must be tested in the termination condition:

;; when using cdr, test termination with null? and
;; when using sub1, test termination with zero?

(define (occur* a l)
  (cond [(null? l) 0]
        [(atom? (car l))
         (cond [(eq? a (car l)) (add1 (occur* a (cdr l)))]
               [else (occur* a (cdr l))])]
        [else (my-add (occur* a (car l))
                      (occur* a (cdr l)))]))

(check-equal? (occur* 'x '()) 0)
(check-equal? (occur* 'x (list 'x)) 1)
(check-equal? (occur* 'x (list (list 'x))) 1)
(check-equal? (occur* 'x (list (list (list (list 'x))))) 1)

(define (subst* new old l)
  (cond [(null? l) '()]
        [(atom? (car l))
         (cond [(eq? (car l) old) (cons new (subst* new old (cdr l)))]
               [else (cons (car l) (subst* new old (cdr l)))])]
        [else (cons (subst* new old (car l))
                    (subst* new old (cdr l)))]))

(check-equal? (subst* 'x 'b (list (list 'b 'b)))
              (list (list 'x 'x)))

(define (insertL* new old l)
  (cond [(null? l) '()]
        [(atom? (car l))
         (cond [(eq? old (car l)) (cons new (cons old (cdr l)))]
               [else (cons (car l) (insertL* (new old (cdr l))))])]
        [else (cons (insertL* new old (car l))
                    (insertL* new old (cdr l)))]))

(check-equal? (insertL* 0 1 (list (list 1 2 3)))
              (list (list 0 1 2 3)))

(define (member* a l)
  (cond [(null? l) false]
        [(atom? (car l))
         (cond [(eq? a (car l)) true]
               [else (member* a (cdr l))])]
        [else (or (member* a (car l))
                  (member* a (cdr l)))]))

(check-equal? (member* 'x (list (list (list 'x)))) true)

(define (leftmost l)
  (cond [(atom? (car l)) (car l)]
        [else (leftmost (car l))]))

(check-equal? (leftmost (list (list 'x))) 'x)
(check-equal? (leftmost (list (list (list (list 'x))))) 'x)

(define (equan? a1 a2)
  (cond [(and (number? a1) (number? a2)) (= a1 a2)]
        [(or (number? a1) (number? a2)) false]
        [else (eq? a1 a2)]))

(check-equal? (equan? 1 1) true)
(check-equal? (equan? 1 2) false)
(check-equal? (equan? 'x 'x) true)
(check-equal? (equan? 'x 'y) false)

(define (eqlist? l1 l2)
  (cond [(and (null? l1) (null? l2)) true]
        [(or (null? l1) (null? l2)) false]
        [(and (atom? (car l1))
              (atom? (car l2)))
         (and (equan? (car l1) (car l2))
              (eqlist? (cdr l1) (cdr l2)))]
        [(or (atom? (car l1)) (atom? (car l2))) false]
        [else (and (eqlist? (car l1) (car l2))
                   (eqlist? (cdr l1) (cdr l2)))]))

(check-equal? (eqlist? empty empty) true)
(check-equal? (eqlist? empty (list 'x)) false)
(check-equal? (eqlist? (list 'x) empty) false)
(check-equal? (eqlist? (list 'x) (list (list 'x))) false)
(check-equal? (eqlist? (list (list 'x)) (list 'x)) false)
(check-equal? (eqlist? (list (list 'x))
                      (list (list 'x))) true)
(check-equal? (eqlist? (list (list (list 'we 'are 'the 'same)))
                       (list (list (list 'we 'are 'the 'same)))) true)
