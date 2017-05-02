#lang racket

(require 2htdp/image "posn.rkt" "defs.rkt")

(provide make-obstacle OBSTACLE-IMG)

(define OBSTACLE-IMG (bitmap "graphics/obstacle.png"))

(define (make-obstacle)
  (posn (add1 (random (sub1 SIZE)))
        (add1 (random (sub1 SIZE)))))
