#lang racket

(require "roguet.rkt")

(struct actor (x y ch col)  #:transparent)
(struct monster actor (strength))

(define violet 1222)

(define player (actor 10 10 "@" violet))

(define orc (monster 10 10 '@ violet 4))

(define monsters (list orc))

(actor? player)

(terminal-open)
(terminal-set8 "window: size=80x30")
(terminal-set8 "font: VeraMoBd.ttf, (size=12")
(terminal-print8 (actor-x player) (actor-y player) (actor-ch player))
(terminal-refresh)
; (terminal-state 10)
(define key (terminal-read)) ; block until input is received.
(display '("You pressed:" (number->string key)))
(terminal-print8 (+ 5 (actor-x player)) (actor-y player) (actor-ch player))
(terminal-delay 1900) ; delay for 1/10 second
(terminal-close)
