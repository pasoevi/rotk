#lang racket/base
(provide posn posn-x posn-y posn-mv posn=?)

(struct posn (x y))

(define (posn-mv p dx dy)
  (posn (+ (posn-x p) dx)
	(+ (posn-y p) dy)))

(define (posn=? p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))
