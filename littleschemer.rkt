#lang racket
(require rackunit)

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

(check-eq? (lat? (list 'bacon (list 'and 'eggs))) false)
(check-eq? (lat? (list 'bacon 'and 'eggs)) true)

(check-eq? (lat? '()) true)
(check-eq? (lat? (list 1 2 3)) true)
(check-eq? (lat? (list 'a 'b 'c)) true)
(check-eq? (lat? (list 1 (list))) false)

;; Atom ListOfAtom -> Boolean
;; produces true if an atom belongs to the provided list and false otherwise
(define (member? a loa)
  (cond [(null? loa) false]
        [(eq? a (car loa)) true]
        [else (member? a (cdr loa))]))

(check-eq? (member? 'poached
                    (list 'fried 'eggs 'scrambled 'eggs)) false)
(check-eq? (member? 'meat
                   (list 'mashed 'potatoes 'and 'meat 'gravy)) true)

(check-eq? (member? 'a (list)) false)
(check-eq? (member? 'a (list 'a)) true)
(check-eq? (member? 'a (list 1 'b 'a)) true)

;; CONS THE MAGNIFICIENT

;; Atom ListOfAtom -> ListOfAtom
;; removes the provided atom and produces a new ListOfAtom

(define (rember a loa)
  (cond [(null? loa) true]
        [(eq? (car loa) a) (cdr loa)]
        [else (cons (car loa) (rember a (cdr loa)))]))

(check-equal? (rember 'mint (list 'lamb 'chops 'and 'mint 'jelly))
           (list 'lamb 'chops 'and 'jelly) "comparing lists")

