#lang racket

(require 2htdp/image 2htdp/universe)
(require "goo.rkt" "posn.rkt" "defs.rkt" "obstacle.rkt")

;; Data definitions

(struct snake (heading segs))
(struct pit (snake obstacles goos))

;; Constants 
(define TICK-RATE 1/10)

;; Snake constansts
(define SEG-SIZE 15)


;; Graphical board
(define WIDTH-PX (* SEG-SIZE 30))
(define HEIGHT-PX (* SEG-SIZE 30))

;; Visual constants
(define MT-SCENE (empty-scene WIDTH-PX HEIGHT-PX))
(define SEG-IMG (bitmap "graphics/body.gif"))
(define HEAD-IMG (bitmap "graphics/head.gif"))
(define HEAD-LEFT-IMG HEAD-IMG)
(define HEAD-DOWN-IMG (rotate 90 HEAD-LEFT-IMG))
(define HEAD-RIGHT-IMG (flip-horizontal HEAD-LEFT-IMG))
(define HEAD-UP-IMG (flip-vertical HEAD-DOWN-IMG))

(define ENDGAME-TEXT-SIZE 35)

(define KEY-UP "up")
(define KEY-RIGHT "right")
(define KEY-DOWN "down")
(define KEY-LEFT "left")

;; main function

;; Start the Game
(define (start-snake)
  (big-bang (pit (snake "right" (list (posn 1 1)))
                 (list (make-obstacle))
                 (list (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)))
            (on-tick next-pit TICK-RATE)
            (on-key direct-snake)
            (to-draw render-pit)
            (stop-when dead? render-end)))

;; Pit -> Pit
;; Take one step: eat or slither
(define (next-pit w)
  (define snake (pit-snake w))
  (define obstacles (pit-obstacles w))
  (define goos (pit-goos w))
  (define goo-to-eat (can-eat snake goos))
  (if goo-to-eat
    (if (= (goo-kind goo-to-eat) GOO-KIND-NORMAL)
        (pit (grow snake) obstacles (age-goo (eat goo-to-eat goos)))
        (pit (grow (grow snake)) obstacles (age-goo (eat goo-to-eat goos))))
    (pit (slither snake) obstacles (age-goo goos))))

;; Pit KeyEvent -> Pit
;; Handle a key event
(define (direct-snake w ke)
  (cond [(dir? ke) (world-change-dir w ke)]
	[else w]))

(define (snake-head sn)
  (car (snake-segs sn)))

(define (snake-body sn)
  (cdr (snake-segs sn)))

(define (snake-tail sn)
  (last (snake-segs sn)))

(define (snake-change-dir sn d)
  (snake d (snake-segs sn)))

(define (dir? x)
  (or (key=? x KEY-UP)
      (key=? x KEY-RIGHT)
      (key=? x KEY-DOWN)
      (key=? x KEY-LEFT)))

(define (opposite-dir? d1 d2)
  (cond [(string=? d1 "up") (string=? d2 "down")]
	[(string=? d1 "right") (string=? d2 "left")]
	[(string=? d1 "down") (string=? d2 "up")]
	[(string=? d1 "left") (string=? d2 "right")]))

(define (world-change-dir w d)
  (define the-snake (pit-snake w))
  (define obstacles (pit-obstacles w))
  (cond [(and (opposite-dir? (snake-heading the-snake) d)
	      ;; consists of the head and at least one segment
	      (cons? (cdr (snake-segs the-snake))))
	 (stop-with w)]
	[else
	  (pit (snake-change-dir the-snake d) obstacles (pit-goos w))]))

(define (can-eat snake goos)
  (cond [(empty? goos) #f]
	[else (if (close? (snake-head snake) (car goos))
		(car goos)
		(can-eat snake (cdr goos)))]))

(define (close? s g)
  (posn=? s (goo-loc g)))

(define (eat goo-to-eat goos)
  (cons (fresh-goo) (remove goo-to-eat goos)))

(define (next-head sn)
  (define head (snake-head sn))
  (define heading (snake-heading sn))
  (cond [(string=? heading "up") (posn-mv head 0 -1)]
	[(string=? heading "right") (posn-mv head 1 0)]
	[(string=? heading "down") (posn-mv head 0 1)]
	[(string=? heading "left") (posn-mv head -1 0)]))


(define (grow sn)
  (snake (snake-heading sn)
	 (cons (next-head sn) (snake-segs sn))))

(define (drop-tail segs)
  (cond [(empty? (cdr segs)) empty] 
	[else (cons (car segs) (drop-tail (cdr segs)))]))

(define (slither sn)
  (snake (snake-heading sn)
	 (cons (next-head sn) (drop-tail (snake-segs sn)))))

; collision functions

(define (self-colliding? snake)
  (cons? (member (snake-head snake) (snake-body snake))))

(define (obstacle-colliding? snake obstacles)
  (not (empty? (filter (lambda (x) (posn=? (snake-head snake) x)) obstacles))))

(define (wall-colliding? snake)
  (define x (posn-x (snake-head snake)))
  (define y (posn-y (snake-head snake)))
  (or (= 0 x) (= x SIZE)
      (= 0 y) (= y SIZE)))

(define (dead? w)
  (define snake (pit-snake w))
  (define obstacles (pit-obstacles w))
  (or (self-colliding? snake) (wall-colliding? snake) (obstacle-colliding? snake obstacles)))

; render functioons
(define (img+scene posn img scene)
  (place-image img
	       (* (posn-x posn) SEG-SIZE)
	       (* (posn-y posn) SEG-SIZE)
	       scene))

#|
(define (img-list+scene posns img scene)
  (cond [(empty? posns) scene]
	[else (img+scene
		(car posns)
		img
		(img-list+scene (cdr posns) img scene))]))
|#

(define (img-list+scene posns img scene)
  (foldr (lambda (p s) (img+scene p img s)) scene posns))
  
  
(define (snake+scene snake scene)
  (define snake-body-scene
    (img-list+scene (snake-body snake) SEG-IMG scene))
  (define dir (snake-heading snake))
  (img+scene (snake-head snake)
	     (cond [(string=? dir "up") HEAD-UP-IMG]
		   [(string=? dir "right") HEAD-RIGHT-IMG]
		   [(string=? dir "down") HEAD-DOWN-IMG]
		   [(string=? dir "left") HEAD-LEFT-IMG])
	     snake-body-scene))

(define (obstacles+scene obstacles scene)
    (img-list+scene obstacles OBSTACLE-IMG scene))

(define (goo-list+scene goos scene)
  (cond [(empty? goos) scene]
	[else
 	 (define goo (car goos))  
  		(img+scene
		(goo-loc goo)
		(cond [(= (goo-kind goo) GOO-KIND-NORMAL) GOO-IMG]
		      [else MEGA-GOO-IMG])
		(goo-list+scene (cdr goos) scene))]))

(define (render-pit w)
  (snake+scene (pit-snake w)
	       (obstacles+scene (pit-obstacles w) (goo-list+scene (pit-goos w) MT-SCENE))))
  
(define (render-end w)
  (define snake (pit-snake w))
  (overlay (above (text "Game Over" ENDGAME-TEXT-SIZE "black")
             (beside (text "Your Score " ENDGAME-TEXT-SIZE "black")
	   (text (number->string (length (snake-body snake))) ENDGAME-TEXT-SIZE "green")))
	   (render-pit w)))
(start-snake)
#|
;; Tests
  
;; Initial Structures

(define goo-list (build-list MAX-GOO (lambda (x) (fresh-goo))))
(define snake0 (snake "right" (list (posn 1 1)))) ;; BUG? << -- moving this define into the test module blows up
(define world0 (pit snake0 goo-list))

(define left-snake0 (snake "left" (list (posn 1 1))))
(define left-world0 (pit left-snake0 goo-list))

;; Test Constants

(define snake1 (snake "left" (list (posn 5 5))))
(define snake2 (snake "up" (list (posn 2 2) (posn 2 3) (posn 2 4))))
(define wall-snake (snake "right" (list (posn 0 1) (posn 1 1))))
(define self-eating-snake (snake "up" (list
                                       (posn 19 3)
                                       (posn 19 4)
                                       (posn 20 4)
                                       (posn 21 4)
                                       (posn 22 4)
                                       (posn 22 3)
                                       (posn 21 3)
                                       (posn 20 3)
                                       (posn 19 3)
                                       (posn 18 3))))
(define goo1 (goo (posn 5 5) 45))
(define goo2 (goo (posn 4 8) 1))
(define goo3 (goo (posn 6 9) 40))
(define goo4 (goo (posn 1 1) 120))
(define goo5 (goo (posn 1 9) 58))
(define goo-list1 (list goo1 goo2 goo3 goo4 goo5))
(define world1 (pit snake1 goo-list1))

(define world2 (pit snake2 goo-list1))

(define right-snake1 (snake "right" (list (posn 5 5))))
(define right-world1 (pit right-snake1 goo-list1))

(module+ test 
  
  (require rackunit rackunit/text-ui)
  
  ;; test the major basic snake functions
  (check-equal? (pit-snake (next-pit world2))
                (snake "up" (list (posn 2 1) (posn 2 2) (posn 2 3))))
  (check-equal? (pit-snake (next-pit world1))
                (snake "left" (list (posn 4 5) (posn 5 5))))
  (check-true (let ([f (pit-goos (next-pit world1))])
                (= (length f) MAX-GOO)))
  (check-equal? (pit-snake (next-pit world0))
                (snake "right" (list (posn 2 1))))
  (check-equal? (pit-snake (next-pit (pit snake0 (list (goo (posn 1 1) 130)))))
                (snake "right" (list (posn 2 1) (posn 1 1))))
  
  (check-equal? (direct-snake world0 "down") 
                (world-change-dir world0 "down"))
  (check-equal? (direct-snake world0 "a")
                world0)
  
  (check-equal? (render-pit world0)
                (snake+scene snake0
                             (goo-list+scene goo-list MT-SCENE)))
  (check-equal? (render-pit world1)
                (snake+scene snake1 (goo-list+scene goo-list1 MT-SCENE)))
  (check-equal? (render-pit world2)
                (snake+scene snake2 (goo-list+scene goo-list1 MT-SCENE)))
  
  (check-true (dead? (pit wall-snake '())))
  (check-true (dead? (pit self-eating-snake  '())))
  (check-false (dead? (pit snake1  '())))
  (check-false (dead? (pit snake2  '())))
  (check-false (dead? world0))
  
  (check-equal? (render-end world1)
                (overlay (text "Game over" 15 "black")
                         (render-pit world1)))
  (check-equal? (render-end world2)
                (overlay (text "Game over" 15 "black")
                         (render-pit world2)))
  
  ;; Properties
  ;; -----------------------------------------------------------------------------
  
  ;; Property: each goo in the list has its 'expire' field decremented by 1
  (define (prop:goo-rot-- i)
    (test-begin 
     (for ([i (in-range i)])
       (define goos (list-of-n-goo MAX-GOO))
       (define goo-initial-expire (map goo-expire goos))
       (check-equal? (map sub1 goo-initial-expire)
                     (map goo-expire (age-goo goos))))))
  
  ;; Property: The position of the goo is:
  ;;  - x in (0,WIDTH-SEGS),
  ;;  - y in (0,HEIGHT-SEGS).
  (define (prop:new-goo-range i)
    (test-begin 
     (for ([i (in-range i)])
       (define f (fresh-goo))
       (check-true (and (< 0 (posn-x (goo-loc f)) SIZE)
                        (< 0 (posn-y (goo-loc f)) SIZE))))))
  
  ;; Number -> [Listof Goo]
  ;; creates a list of randomly selected goo that is n long.
  (define (list-of-n-goo n)
    (cond [(zero? n) empty]
          [else (define rand (random 5))
                (cons (list-ref goo-list1 rand) (list-of-n-goo (sub1 n)))]))
  
  ;; testing pit-snake event handling 
  
  (check-equal? (pit-snake (world-change-dir (pit snake1 "foobar") "down"))
                (snake "down" (list (posn 5 5))))
  (check-equal? (pit-snake (world-change-dir (pit snake2 "left") "left"))
                (snake "left" (list (posn 2 2) (posn 2 3) (posn 2 4))))
  
  (prop:goo-rot-- 1000)
  
  (check-equal? (grow snake0)
                (snake "right" (list (posn 2 1) (posn 1 1))))
  (check-equal? (grow snake1)
                (snake "left" (list (posn 4 5) (posn 5 5))))
  (check-equal? (grow snake0)
                (snake "right" (list (posn 2 1) 
                                     (posn 1 1))))
  
  (prop:new-goo-range 1000)
  
  (check-equal? (can-eat (snake "right" `(,(posn 3 3))) `(,(goo (posn 3 3) 130)))
                (goo (posn 3 3) 130))
  (check-false (can-eat (snake "right" `(,(posn 3 3))) `(,(goo (posn 3 4) 130)
                                                         ,(goo (posn 2 2) 0))))
  (check-equal? (can-eat snake0 (list (goo (posn 1 1) 1)))
                (goo (posn 1 1) 1))
  (check-false (can-eat snake0 (list (goo (posn 2 1) 1))))
  
  (check-equal? (slither snake0) (snake "right" (list (posn 2 1))))
  (check-equal? (slither (snake "right" (list (posn 4 4) 
                                              (posn 4 5)
                                              (posn 4 6))))
                (snake "right" (list (posn 5 4) (posn 4 4) (posn 4 5))))
  (check-equal? (slither snake0)
                (snake "right" (list (posn 2 1))))
  
  (check-equal? (length (eat (goo (posn 1 1) 130) (list (goo (posn 1 1) 130))))
                1)
  (check-equal? (grow (snake "left" (list (posn 1 1))))
                (snake "left" (list (posn 0 1) (posn 1 1))))
  
  (check-equal? (next-head snake0) (posn 2 1))
  (check-equal? (next-head (snake "left" (list (posn 1 1))))
                (posn 0 1))
  (check-equal? (next-head (snake "up" (list (posn 1 1))))
                (posn 1 0))
  (check-equal? (next-head (snake "down" (list (posn 1 1)))) 
                (posn 1 2))
  (check-equal? (next-head snake0) (posn 2 1))
  
  (check-equal? (posn-mv (posn 1 1) 2 3) (posn 3 4))
  (check-equal? (posn-mv (posn 3 4) 6 0) (posn 9 4))
  (check-equal? (posn-mv (posn 2 8) 0 5) (posn 2 13))
  (check-equal? (posn-mv (posn 2 3) 0 0) (posn 2 3))
  
  (check-equal? (drop-tail '(1 2 3 4 5 6))
                '(1 2 3 4 5))
  (check-equal? (drop-tail (snake-segs snake2))
                `(,(posn 2 2) ,(posn 2 3)))
  (check-equal? (drop-tail (list 0)) empty)
  (check-equal? (drop-tail (list 0 1 2)) (list 0 1))
  
  ;; testing snake-key-handling
  
  (check-true (dir? "up"))
  (check-true (dir? "down"))
  (check-true (dir? "left"))
  (check-true (dir? "right"))
  (check-false (dir? "f"))
  (check-true (dir? "right"))
  
  (check-equal? (world-change-dir world1 "left") world1)
  (check-equal? (world-change-dir world1 "right") right-world1)
  (check-equal? (world-change-dir world0 "left") left-world0)
  (check-equal? (world-change-dir world0 "right") 
                (pit (snake "right" (snake-segs (pit-snake world0)))
                     (pit-goos world0)))
  (check-equal? (world-change-dir world0 "down")
                (pit (snake "down" (snake-segs (pit-snake world0)))
                     (pit-goos world0)))
  
  (check-true (opposite-dir? "up" "down"))
  (check-true (opposite-dir? "left" "right"))
  (check-true (opposite-dir? "right" "left"))
  (check-true (opposite-dir? "down" "up"))
  (check-false (opposite-dir? "left" "down"))
  (check-false (opposite-dir? "right" "down"))
  (check-false (opposite-dir? "down" "left"))
  (check-false (opposite-dir? "up" "right"))
  (check-true (opposite-dir? "up" "down"))
  (check-true (opposite-dir? "down" "up"))
  (check-false (opposite-dir? "up" "up") false)
  (check-equal? (opposite-dir? "right" "left") true)
  (check-equal? (opposite-dir? "left" "right") true)
  
  ;; testing snake rendering
  
  (check-equal? (snake+scene snake1 MT-SCENE)
                (place-image HEAD-LEFT-IMG (* 5 SEG-SIZE)
                             (* 5 SEG-SIZE) MT-SCENE))
  (check-equal? (snake+scene snake2 MT-SCENE)
                (img+scene (posn 2 2) HEAD-UP-IMG 
                           (img+scene (posn 2 3) SEG-IMG 
                                      (img+scene (posn 2 4) SEG-IMG MT-SCENE))))
  (check-equal? (snake+scene (snake "up" (list (posn 1 1))) MT-SCENE)
                (img+scene (posn 1 1) HEAD-UP-IMG MT-SCENE))
  
  (check-equal? (goo-list+scene (list goo1) MT-SCENE)
                (place-image GOO-IMG (* 5 SEG-SIZE)
                             (* 5 SEG-SIZE) MT-SCENE))
  (check-equal? (goo-list+scene goo-list1 MT-SCENE)
                (img-list+scene (list (posn 5 5) (posn 4 8) (posn 6 9) (posn 1 1) (posn 1 9))
                                GOO-IMG MT-SCENE))
  
  (check-equal? (img-list+scene (list (posn 3 3) (posn 4 4)) SEG-IMG MT-SCENE)
                (place-image SEG-IMG (* 3 SEG-SIZE) (* 3 SEG-SIZE) 
                             (place-image SEG-IMG (* 4 SEG-SIZE) (* 4 SEG-SIZE) MT-SCENE)))
  (check-equal? (img-list+scene (list (posn 1 1) (posn 10 10)) SEG-IMG MT-SCENE)
                (place-image SEG-IMG (* 1 SEG-SIZE) (* 1 SEG-SIZE)  
                             (place-image SEG-IMG (* 10 SEG-SIZE) (* 10 SEG-SIZE) MT-SCENE)))
  (check-equal? (img-list+scene (list (posn 1 1)) GOO-IMG MT-SCENE)
                (place-image GOO-IMG SEG-SIZE SEG-SIZE
                             (img-list+scene empty GOO-IMG MT-SCENE)))
  
  (check-equal? (img+scene (posn 4 3) SEG-IMG MT-SCENE)
                (place-image SEG-IMG (* 4 SEG-SIZE) (* 3 SEG-SIZE)  MT-SCENE))
  (check-equal? (img+scene (posn 5 2) GOO-IMG MT-SCENE)
                (place-image GOO-IMG (* 5 SEG-SIZE) (* 2 SEG-SIZE)  MT-SCENE))
  (check-equal? (img+scene (posn 1 1) SEG-IMG MT-SCENE)
                (place-image SEG-IMG SEG-SIZE SEG-SIZE MT-SCENE))
  
  ;; testing the endgame
  (check-false (self-colliding? snake1))
  (check-false (self-colliding? snake2))
  (check-false (self-colliding? wall-snake))
  (check-true (self-colliding? self-eating-snake))
  (check-false (self-colliding? snake0))
  (check-true (self-colliding? (snake (snake-heading snake0)
                                      (cons (posn 1 1) 
                                            (snake-segs snake0)))))
  
  (check-false (wall-colliding? snake1))
  (check-false (wall-colliding? snake2))
  (check-false (wall-colliding? self-eating-snake))
  (check-true (wall-colliding? wall-snake))
  (check-true 
   (wall-colliding? (snake "right" (list (posn (/ WIDTH-PX SEG-SIZE) 0)))))
  (check-true 
   (wall-colliding? (snake "down" (list (posn 0 (/ HEIGHT-PX SEG-SIZE))))))
  (check-true 
   (wall-colliding? (snake "up" (list (posn 1 0)))))
  (check-equal? (wall-colliding? (snake "right" 
                                        (list (posn 0 1))))
                true)
  (check-equal? (wall-colliding? (snake "right" 
                                        (list (posn 1 0))))
                true)
  (check-equal? (wall-colliding? (snake "right" 
                                        (list (posn 1 1))))
                false)
  (check-true (wall-colliding? (snake "right" (list (posn 1 SIZE)))))
  
  
  ;; testing utilities functions 
  
  (check-false (posn=? (posn 1 1) (posn 2 2)))
  (check-false (posn=? (posn 1 2) (posn 2 1)))
  (check-true (posn=? (posn 3 4) (posn 3 4)))
  (check-true (posn=? (posn 2 2) (posn 2 2)))
  (check-equal? (posn=? (posn 1 2) (posn 1 1)) false)
  (check-equal? (posn=? (posn 1 2) (posn 1 2)) true)
  (check-equal? (posn-mv (posn 0 0) 2 3) (posn 2 3))    
  
  (check-equal? (snake-head snake1) (posn 5 5))
  (check-equal? (snake-head snake2) (posn 2 2))
  (check-equal? (snake-head snake0) (posn 1 1))
  
  (check-equal? (snake-body snake1) empty)
  (check-equal? (snake-body snake0) empty)
  (check-equal? (snake-body snake2) (list (posn 2 3) (posn 2 4)))
  
  (check-equal? (snake-change-dir snake0 "up") 
                (snake "up" (list (posn 1 1))))
  (check-equal? (snake-change-dir snake1 "down") 
                (snake "down" (list (posn 5 5))))
  (check-equal? (snake-change-dir snake2 "left") 
                (snake "left" (list (posn 2 2) (posn 2 3) (posn 2 4))))
  
  (check-true (rotten? (goo (posn 1 2) 0)))
  (check-true (rotten? (goo (posn 6 9) 0)))
  (check-true (rotten? (goo (posn 23 2) 0)))
  
  (check-false (rotten? (goo (posn 1 2) 2)))
  (check-false (rotten? (goo (posn 3 45) 45334534)))
  (check-false (rotten? (goo (posn 2 4) 9)))
  
  (check-equal? (decay (goo (posn 1 2) 2))
                (goo (posn 1 2) 1))
  (check-equal? (decay (goo (posn 132 0) 2))
                (goo (posn 132 0) 1))
  (check-equal? (decay (goo (posn 1 2) 10))
                (goo (posn 1 2) 9))
  (check-equal? (decay (goo (posn 3 5) 8))
                (goo (posn 3 5) 7))
  
  "all tests run")

|#
