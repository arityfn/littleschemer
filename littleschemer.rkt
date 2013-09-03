#lang racket
(require rackunit)

;; ListOfBooleans -> Boolean
;; takes a listOfBooleans and produces true if all the elements
;; are true and false otherwise
(define (my-and xs) (foldr (lambda (a b) (and a b)) true xs))

(check-equal? (my-and (list true true true)) true)
(check-equal? (my-and (list true true false true)) false)

;; (a -> Boolean) ListOfA -> Boolean 
;; applied to a predicate and a list. returns true if any of the
;; elements of the list satisfy the predicate and false otherwise.
(define (any p xs) (my-or (map p xs)))

;; ListOfBooleans -> Boolean
;; applied to a list of booleans. returns their logical disjunction.
(define (my-or xs) (foldr (lambda (a b) (or a b)) false xs))

(check-equal? (my-or (list false false false)) false)
(check-equal? (my-or (list false true false false)) true)

;; (a -> Boolean)  ListOfA -> Boolean
;; takes a predicate and a list, returns true if all elements of the list
;; satisfy the predicate and false otherwiese.
(define (all p xs) (my-and (map p xs)))

(check-equal? (all (lambda (x) (< x 11)) (range 0 11)) true)

;; Atom -> Boolean
;; produces true if the argument provided is an atom and false otherwise
(define (atom? x)
  (and (not (pair? x)) (not (null? x))))

(check-eq? (atom? (quote ())) false)
(check-eq? (atom? 'a) true)
(check-eq? (atom? (cons 'b empty)) false)
(check-eq? (atom? 1) true)

;; ListOfAtom -> Boolean
;; produces true if a list consists only of atoms
(define (lat? loa)
  (cond [(null? loa) true]
        [(atom? (car loa)) (lat? (cdr loa))]
        [else false]))

;; with pattern matching
;; ListOfAtom -> Boolean
;; produces true if a list consists only of atoms
(define (lat-2? loa)
  (match loa
         ['() true]
         [(cons hd tl) (if (atom? hd)
                         (lat-2? tl)
                         false)]))

;; abstacting it with all 
;; ListOfAtom -> Boolean
;; produces true if a list consists only of atoms
(define (lat-3? loa)
  (all (lambda (x) (atom? x)) loa))

(check-eq? (lat? (list 'bacon (list 'and 'eggs))) false)
(check-eq? (lat? (list 'bacon 'and 'eggs)) true)
(check-eq? (lat-2? (list 'bacon (list 'and 'eggs))) false)
(check-eq? (lat-2? (list 'bacon 'and 'eggs)) true)
(check-eq? (lat-3? (list 'bacon (list 'and 'eggs))) false)
(check-eq? (lat-3? (list 'bacon 'and 'eggs)) true)

;; Atom ListOfAtom -> Boolean
;; produces true if an atom belongs to the provided list and false otherwise
(define (member? a loa)
  (cond [(null? loa) false]
        [(eq? a (car loa)) true]
        [else (member? a (cdr loa))]))

;; with pattern matching
;; Atom ListOfAtom -> Boolean
;; produces true if an atom belongs to the provided list and false otherwise
(define (member-2? a loa)
  (match loa
         ['()           false]
         [(cons hd tl)  (cond [(eq? a hd) true]
                              [else (member-2? a tl)])]))
;; abstracting it with any
;; Atom ListOfAtom -> Boolean
;; produces true if an atom belongs to the provided list and false otherwise
(define (member-3? a loa)
  (any (lambda (x) (eq? x a)) loa))

(check-eq? (member? 'poached (list 'fried 'eggs 'scrambled 'eggs)) false)
(check-eq? (member? 'meat (list 'mashed 'potatoes 'and 'meat 'gravy)) true)
(check-eq? (member-2? 'poached (list 'fried 'eggs 'scrambled 'eggs)) false)
(check-eq? (member-2? 'meat (list 'mashed 'potatoes 'and 'meat 'gravy)) true)
(check-eq? (member-3? 'poached (list 'fried 'eggs 'scrambled 'eggs)) false)
(check-eq? (member-3? 'meat (list 'mashed 'potatoes 'and 'meat 'gravy)) true)

;; CONS THE MAGNIFICIENT

;; Atom ListOfAtom -> ListOfAtom
;; removes the provided atom and produces a new ListOfAtom
(define (rember a loa)
  (cond [(null? loa) true]
        [(eq? (car loa) a) (cdr loa)]
        [else (cons (car loa) (rember a (cdr loa)))]))

(check-equal? (rember 'mint (list 'lamb 'chops 'and 'mint 'jelly))
           (list 'lamb 'chops 'and 'jelly) "comparing lists")

;; ListOfLists -> ListOfAtoms
;; takes a list of lists and produces a new list with the first s-expression
;; of each internal list
(define (firsts lol)
  (cond [(null? lol) '()]
        [(cons (car (car lol)) (firsts (cdr lol)))]))

(check-equal? (firsts empty) '())
(check-equal? (firsts (list (list 1 2))) (list 1))
(check-equal? (firsts (list (list (list'five 'plums) 'four)
             (list 'eleven 'green 'oranges)
             (list (list 'no) 'more))) (list (list 'five 'plums)
                                             'eleven (list 'no)))

;; atom atom ListOfAtoms -> ListOfAtoms
;; produces a new list in which the new atom is inserted to the right of the
;; old atom
(define (insertR n o loa)
  (cond [(null? loa) empty]
        [(equal? o (car loa)) (cons o (cons n (cdr loa)))]
        [else (cons (car loa) (insertR n o (cdr loa)))]))

(check-equal? (insertR 'topping 'fudge (list 'ice 'cream 'with 'fudge 'for 'dessert))
              (list 'ice 'cream 'with 'fudge 'topping 'for 'dessert))

;; atom atom ListoOfAtoms -> ListOfAtoms
;; produces a new list in which t)) new atom is inserted to the left of the old atom
(define (insertL n o loa)
  (cond [(null? loa) empty]
        [(equal? o (car loa)) (cons n (cons o (cdr loa)))]
        [else (cons (car loa) (insertL n o (cdr loa)))]))

(check-equal? (insertL 0 1 (list 1 2 3 4 5))
              (list 0 1 2 3 4 5))

(define (subst n o loa)
  (cond [(null? loa) empty]
        [(eq? o (car loa)) (cons n (cdr loa))]
        [else (cons (car loa) (subst n o (cdr loa)))]))

(check-equal? (subst 'topping 'fudge (list 'ice 'cream 'with 'fudge 'for 'dessert))
              (list 'ice 'cream 'with 'topping 'for 'dessert))

(define (subst2 n o1 o2 loa)
  (cond [(null? loa) empty]
        [(or (eq? o1 (car loa)) (eq? o2 (car loa))) (cons n (cdr loa))]
        [else (cons (car loa) (subst2 n o1 o2 (cdr loa)))]))

(check-equal? (subst2 'vanilla 'cholate 'banana (list 'banana 'ice 'cream 'with
                      'chocolate 'topping)) (list 'vanilla 'ice 'cream 'with 'chocolate 'topping))

(define (multirember a lat)
  (cond [(null? lat) empty]
        [(eq? (car lat) a) (multirember a (cdr lat))]
        [else (cons (car lat) (multirember a (cdr lat)))]))

(check-equal? (multirember 1 (list 1 2 1 2 1 2))
              (list 2 2 2))
(check-equal? (multirember 'cup (list 'coffee 'cup 'tea 'cup 'and 'hick 'cup))
              (list 'coffee 'tea 'and 'hick))

(define (multiinsertR n o lat)
  (cond [(null? lat) empty]
        [(eq? o (car lat)) (cons o (cons n (multiinsertR n o (cdr lat))))]
        [else (cons (car lat) (multiinsertR n o (cdr lat)))]))

(check-equal? (multiinsertR 1 2 (list 2 2 2)) (list 2 1 2 1 2 1))

(define (multiinsertL n o lat)
  (cond [(null? lat) empty]
        [(eq? o (car lat)) (cons n (cons o (multiinsertL n o (cdr lat))))]
        [else (cons (car lat) (multiinsertL n o (cdr lat)))]))

(check-equal? (multiinsertL 1 2 (list 2 2 2)) (list 1 2 1 2 1 2))

(define (multisubst n o lat)
  (cond [(null? lat) empty]
        [(eq? o (car lat)) (cons n (multisubst n o (cdr lat)))]
        [else (cons (car lat) (multisubst n o (cdr lat)))]))

(check-equal? (multisubst 2 1 (list 2 1 2 1 2 1)) (list 2 2 2 2 2 2))
