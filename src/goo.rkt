#lang racket

(require 2htdp/image "defs.rkt" "posn.rkt")
(struct goo (loc expire kind))

(provide fresh-goo goo goo-kind goo-loc GOO-KIND-NORMAL age-goo GOO-IMG MEGA-GOO-IMG)

;; Goo constants
(define EXPIRATION-TIME 150)
(define GOO-KIND-NORMAL 1)
(define GOO-KIND-MEGA 2)
(define MAX-GOO 5)

;; Visual constants
(define GOO-IMG (bitmap "graphics/goo.png"))
(define MEGA-GOO-IMG (bitmap "graphics/goo_mega.png"))

(define (rotten? g)
  (zero? (goo-expire g)))

(define (decay g)
  (goo (goo-loc g) (sub1 (goo-expire g)) (goo-kind g)))

#| 
(define (rot goos)
	(map (lambda (g)
               (goo (goo-loc g) (sub1 (goo-expire g))))
               goos))
|#
(define (rot goos)
  (map decay goos))

(define (renew goos)
  (cond [(empty? goos) empty]
	[(rotten? (car goos))
	 (cons (fresh-goo) (renew (cdr goos)))]
	[else
	  (cons (first goos) (renew (cdr goos)))]))

#|
(define (renew goos)
  (filter (lambda (g) (not (rotten? g))) goos))
|#
(define (fresh-goo)
  (goo (posn (add1 (random (sub1 SIZE)))
	     (add1 (random (sub1 SIZE))))
       (add1 (random EXPIRATION-TIME))
       (add1 (random 2))))

(define (age-goo goos)
  (rot (renew goos)))

