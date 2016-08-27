#|

|#

#lang racket

(require ffi/unsafe
         "load.rkt")

(provide (all-defined-out))

(defroguet terminal-open : -> _int)
(defroguet terminal-close : -> _void)
(defroguet terminal-set8 : _string -> _int)
(defroguet terminal-set16 : _pointer -> _int)
(defroguet terminal-set32 : _pointer -> _int)
;(defroguet terminal-set : _string -> _int)
(defroguet terminal-refresh : -> _void)
(defroguet terminal-clear : -> _void)
(defroguet terminal-clear-area : _int _int _int _int -> _void)
(defroguet terminal-crop : _int _int _int _int -> _void)
(defroguet terminal-layer : _int -> _void)

(define _color_t _uint32)
(defroguet terminal-color : _color_t -> _void)
(defroguet terminal-bkcolor : _color_t -> _void)
(defroguet terminal-composition : _int -> _void)
(defroguet terminal-put : _int _int _int -> _void)
(defroguet terminal-put-ext : _int _int _int _int _int _pointer -> _void)
(defroguet terminal-pick : _int _int _int -> _int)
(defroguet terminal-pick-color : _int _int _int -> _color_t)
(defroguet terminal-pick-bkcolor : _int _int -> _color_t)
(defroguet terminal-print8 : _int _int _string -> _int)
(defroguet terminal-print16 : _int _int _pointer -> _int)
(defroguet terminal-print32 : _int _int _pointer -> _int)
;(defroguet terminal-print : _int _int _pointer -> _int) ; need to be defined as macro
(defroguet terminal-measure8 : _pointer -> _int)
(defroguet terminal-measure16 : _pointer -> _int)
(defroguet terminal-measure32 : _pointer -> _int)
(defroguet terminal-has-input : -> _int)
(defroguet terminal-state : _int -> _int)
(defroguet terminal-read : -> _int)
(defroguet terminal-read_str8 : _int _int _pointer _int -> _int)
(defroguet terminal-read_str16 : _int _int _pointer _int -> _int -> _int)
(defroguet terminal-read_str32 : _int _int _pointer _int -> _int -> _int)
(defroguet terminal-peek : -> _int)
(defroguet terminal-delay : _int -> _void)
(defroguet terminal-get8 : _pointer _pointer -> _pointer)
(defroguet terminal-get16 : _pointer _pointer -> _pointer)
(defroguet terminal-get32 : _pointer _pointer -> _pointer)

;color_t color_from_name8(const int8_t* name);
;color_t color_from_name16(const int16_t* name);
;color_t color_from_name32(const int32_t* name);

;; Test

(terminal-open)
(terminal-set8 "window: size=80x30")
(terminal-set8 "font: VeraMoBd.ttf, size=12")
(terminal-print8 1 1 "Гамарджоба, ωōrlд!")
(terminal-refresh)
(terminal-delay 100) ; delay for 1/10 second
;(terminal-read) ; block until input is received. 
(terminal-close)


