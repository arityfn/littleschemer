#lang racket
(require rackunit)

;(define (my-and xs) (foldr (lambda (a b) (and a b)) true xs))
;
;(check-equal? (my-and (list true true true)) true)
;(check-equal? (my-and (list true true false true)) false)
;
;(define (any p xs) (my-or (map p xs)))
;
;(define (my-or xs) (foldr (lambda (a b) (or a b)) false xs))
;
;(check-equal? (my-or (list false false false)) false)
;(check-equal? (my-or (list false true false false)) true)
;
;(define (all p xs) (my-and (map p xs)))
;
;(check-equal? (all (lambda (x) (< x 11)) (range 0 11)) true)
;
;(define (atom? x)
;  (and (not (pair? x)) (not (null? x))))
;
;(check-eq? (atom? (quote ())) false)
;(check-eq? (atom? 'a) true)
;(check-eq? (atom? (cons 'b empty)) false)
;(check-eq? (atom? 1) true)
;
;(define (lat? loa)
;  (cond [(null? loa) true]
;        [(atom? (car loa)) (lat? (cdr loa))]
;        [else false]))
;
;(define (lat-2? loa)
;  (match loa
;         ['() true]
;         [(cons hd tl) (if (atom? hd)
;                         (lat-2? tl)
;                         false)]))
;
;(define (lat-3? loa)
;  (all (lambda (x) (atom? x)) loa))
;
;(check-eq? (lat? (list 'bacon (list 'and 'eggs))) false)
;(check-eq? (lat? (list 'bacon 'and 'eggs)) true)
;(check-eq? (lat-2? (list 'bacon (list 'and 'eggs))) false)
;(check-eq? (lat-2? (list 'bacon 'and 'eggs)) true)
;(check-eq? (lat-3? (list 'bacon (list 'and 'eggs))) false)
;(check-eq? (lat-3? (list 'bacon 'and 'eggs)) true)
;
;(define (member? a loa)
;  (cond [(null? loa) false]
;        [(eq? a (car loa)) true]
;        [else (member? a (cdr loa))]))
;
;(define (member-2? a loa)
;  (match loa
;         ['()           false]
;         [(cons hd tl)  (cond [(eq? a hd) true]
;                              [else (member-2? a tl)])]))
;
;(define (member-3? a loa)
;  (any (lambda (x) (eq? x a)) loa))
;
;(check-eq? (member? 'poached (list 'fried 'eggs 'scrambled 'eggs)) false)
;(check-eq? (member? 'meat (list 'mashed 'potatoes 'and 'meat 'gravy)) true)
;(check-eq? (member-2? 'poached (list 'fried 'eggs 'scrambled 'eggs)) false)
;(check-eq? (member-2? 'meat (list 'mashed 'potatoes 'and 'meat 'gravy)) true)
;(check-eq? (member-3? 'poached (list 'fried 'eggs 'scrambled 'eggs)) false)
;(check-eq? (member-3? 'meat (list 'mashed 'potatoes 'and 'meat 'gravy)) true)
;
;;; CONS THE MAGNIFICIENT
;(define (rember a loa)
;  (cond [(null? loa) true]
;        [(eq? (car loa) a) (cdr loa)]
;        [else (cons (car loa) (rember a (cdr loa)))]))
;
;(check-equal? (rember 'mint (list 'lamb 'chops 'and 'mint 'jelly))
;           (list 'lamb 'chops 'and 'jelly) "comparing lists")
;
;(define (firsts lol)
;  (cond [(null? lol) '()]
;        [(cons (car (car lol)) (firsts (cdr lol)))]))
;
;(check-equal? (firsts empty) '())
;(check-equal? (firsts (list (list 1 2))) (list 1))
;(check-equal? (firsts (list (list (list 'five 'plums) 'four)
;             (list 'eleven 'green 'oranges)
;             (list (list 'no) 'more))) (list (list 'five 'plums)
;                                             'eleven (list 'no)))
;
;(define (insertR n o loa)
;  (cond [(null? loa) empty]
;        [(equal? o (car loa)) (cons o (cons n (cdr loa)))]
;        [else (cons (car loa) (insertR n o (cdr loa)))]))
;
;(check-equal? (insertR 'topping 'fudge (list 'ice 'cream 'with 'fudge 'for 'dessert))
;              (list 'ice 'cream 'with 'fudge 'topping 'for 'dessert))
;
;(define (insertL n o loa)
;  (cond [(null? loa) empty]
;        [(equal? o (car loa)) (cons n (cons o (cdr loa)))]
;        [else (cons (car loa) (insertL n o (cdr loa)))]))
;
;(check-equal? (insertL 0 1 (list 1 2 3 4 5))
;              (list 0 1 2 3 4 5))
;
;(define (subst n o loa)
;  (cond [(null? loa) empty]
;        [(eq? o (car loa)) (cons n (cdr loa))]
;        [else (cons (car loa) (subst n o (cdr loa)))]))
;
;(check-equal? (subst 'topping 'fudge (list 'ice 'cream 'with 'fudge 'for 'dessert))
;              (list 'ice 'cream 'with 'topping 'for 'dessert))
;
;(define (subst2 n o1 o2 loa)
;  (cond [(null? loa) empty]
;        [(or (eq? o1 (car loa)) (eq? o2 (car loa))) (cons n (cdr loa))]
;        [else (cons (car loa) (subst2 n o1 o2 (cdr loa)))]))
;
;(check-equal? (subst2 'vanilla 'cholate 'banana (list 'banana 'ice 'cream 'with
;                      'chocolate 'topping)) (list 'vanilla 'ice 'cream 'with 'chocolate 'topping))
;
;(define (multirember a lat)
;  (cond [(null? lat) empty]
;        [(eq? (car lat) a) (multirember a (cdr lat))]
;        [else (cons (car lat) (multirember a (cdr lat)))]))
;
;(check-equal? (multirember 1 (list 1 2 1 2 1 2))
;              (list 2 2 2))
;(check-equal? (multirember 'cup (list 'coffee 'cup 'tea 'cup 'and 'hick 'cup))
;              (list 'coffee 'tea 'and 'hick))
;
;(define (multiinsertR n o lat)
;  (cond [(null? lat) empty]
;        [(eq? o (car lat)) (cons o (cons n (multiinsertR n o (cdr lat))))]
;        [else (cons (car lat) (multiinsertR n o (cdr lat)))]))
;
;(check-equal? (multiinsertR 1 2 (list 2 2 2)) (list 2 1 2 1 2 1))
;
;(define (multiinsertL n o lat)
;  (cond [(null? lat) empty]
;        [(eq? o (car lat)) (cons n (cons o (multiinsertL n o (cdr lat))))]
;        [else (cons (car lat) (multiinsertL n o (cdr lat)))]))
;
;(check-equal? (multiinsertL 1 2 (list 2 2 2)) (list 1 2 1 2 1 2))
;
;(define (multisubst n o lat)
;  (cond [(null? lat) empty]
;        [(eq? o (car lat)) (cons n (multisubst n o (cdr lat)))]
;        [else (cons (car lat) (multisubst n o (cdr lat)))]))
;
;(check-equal? (multisubst 2 1 (list 2 1 2 1 2 1)) (list 2 2 2 2 2 2))
;
;; chapter 3 - Number games
;(define (- a b)
;  (cond [(zero? b) a]
;        [else (sub1 (- a (sub1 b)))]))
;
;(check-equal? (- 1 1 ) 0)
;(check-equal? (- 2 1 ) 1)
;(check-equal? (- 2 1 ) 1)
;(check-equal? (- 0 0 ) 0)
;
;(define (+ a b)
;  (cond [(zero? b) a]
;        [else (add1 (+ a (sub1 b)))]))
;
;; my version
;(define (my-+ a b)
;  (if (zero? b)
;    a
;    (+ 1 (my-+ a (- b 1)))))
;
;(check-equal? (+ 7 0) 7)
;(check-equal? (+ 7 1) 8)
;(check-equal? (+ 0 0) 0)
;(check-equal? (+ 0 0) 0)
;(check-equal? (+ 0 1) 1)
;
;(check-equal? (my-+ 7 0) 7)
;(check-equal? (my-+ 7 1) 8)
;(check-equal? (my-+ 0 0) 0)
;(check-equal? (my-+ 0 0) 0)
;(check-equal? (my-+ 0 1) 1)
;
;(define (addtup tup)
;  (cond [(null? tup) 0]
;        [else (+ (addtup (cdr tup)) (car tup))]))
;
;(check-equal? (addtup empty) 0)
;(check-equal? (addtup (list 1)) 1)
;(check-equal? (addtup (list 1 2)) 3)
;(check-equal? (addtup (list 1 2 3)) 6)
;(check-equal? (addtup (list 1 2 3 0)) 6)
;
(define (x a b)
  (cond [(zero? b) 0]
        [else (+ a (x a (sub1 b)))]))

;(check-equal? (x 0 0) 0)
;(check-equal? (x 0 1) 0)
;(check-equal? (x 1 1) 1)
;(check-equal? (x 2 1) 2)
;(check-equal? (x 5 5) 25)
;
;(define (tup+ l1 l2)
;  (cond [(and (null? l1)
;              (null? l2)) empty]
;        [else (cons (+ (car l1) (car l2))
;                    (tup+ (cdr l1) (cdr l2)))]))
;
;(check-equal? (tup+ empty empty) empty)
;(check-equal? (tup+ (list 1) (list 1)) (list 2))
;(check-equal? (tup+ (list 2 2 1) (list 2 2 1)) (list 4 4 2))
;
;;; improved tup+ that works on lists of different length
;(define (imp-tup+ l1 l2)
;  (cond [(null? l1) l2]
;        [(null? l2) l1]
;        [else (cons (+ (car l1) (car l2))
;                    (imp-tup+ (cdr l1) (cdr l2)))]))
;
;(check-equal? (imp-tup+ empty empty) empty)
;(check-equal? (imp-tup+ empty (list 1)) (list 1))
;(check-equal? (imp-tup+ (list 2 2) (list 2 2)) (list 4 4))
;
;(define (> a b)
;  (cond [(zero? a) false]
;        [(zero? b) true]
;        [else (> (sub1 a) (sub1 b))]))
;
;(check-equal? (> 2 0) true)
;(check-equal? (> 0 2) false)
;(check-equal? (> 3 3) false)

(define (< a b)
  (cond [(zero? b) false]
        [(zero? a) true]
        [else (< (sub1 a) (sub1 b))]))

(check-equal? (< 0 1) true)
(check-equal? (< 1 0) false)
(check-equal? (< 1 1) false)
(check-equal? (< 6 6) false)

(define (= a b)
  (cond [(and (zero? a) (zero? b)) true]
        [(or (zero? a) (zero? b)) false]
        [else (= (sub1 a) (sub1 b))]))

(check-equal? (= 0 0) true)
(check-equal? (= 1 1) true)
(check-equal? (= 2 1) false)
(check-equal? (= 1 2) false)

(define (pow a b)
  (cond [(zero? b) 1]
        [else (x a (pow a (sub1 b)))]))

(check-equal? (pow 1 1) 1)
(check-equal? (pow 2 3) 8)
(check-equal? (pow 5 3) 125)

(define (/ a b)
  (cond [(< a b) 0]
        [else (add1 (/ (- a b) b))]))

(check-equal? (/ 15 4) 3) 
