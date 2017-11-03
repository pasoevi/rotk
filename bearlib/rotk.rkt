#|

|#

#lang racket

(require ffi/unsafe
         "load.rkt")

(provide (all-defined-out))

(defrotk terminal-open : -> _int)
(defrotk terminal-close : -> _void)
(defrotk terminal-set8 : _string -> _int)
(defrotk terminal-set16 : _pointer -> _int)
(defrotk terminal-set32 : _pointer -> _int)
;(defrotk terminal-set : _string -> _int)
(defrotk terminal-refresh : -> _void)
(defrotk terminal-clear : -> _void)
(defrotk terminal-clear-area : _int _int _int _int -> _void)
(defrotk terminal-crop : _int _int _int _int -> _void)
(defrotk terminal-layer : _int -> _void)

(define _color_t _uint32)
(defrotk terminal-color : _color_t -> _void)
(defrotk terminal-bkcolor : _color_t -> _void)
(defrotk terminal-composition : _int -> _void)
(defrotk terminal-put : _int _int _int -> _void)
(defrotk terminal-put-ext : _int _int _int _int _int _pointer -> _void)
(defrotk terminal-pick : _int _int _int -> _int)
(defrotk terminal-pick-color : _int _int _int -> _color_t)
(defrotk terminal-pick-bkcolor : _int _int -> _color_t)
(defrotk terminal-print-ext8 : _int _int _string -> _int)
(defrotk terminal-print-ext16 : _int _int _pointer -> _int)
(defrotk terminal-print-ext32 : _int _int _pointer -> _int)
;(defrotk terminal-print : _int _int _pointer -> _int) ; needs to be defined as macro
(defrotk terminal-print : _int _int _string -> _int)
(defrotk terminal-measure-ext8 : _pointer -> _int)
(defrotk terminal-measure-ext16 : _pointer -> _int)
(defrotk terminal-measure-ext32 : _pointer -> _int)
(defrotk terminal-has-input : -> _int)
(defrotk terminal-state : _int -> _int)
(defrotk terminal-read : -> _int)
(defrotk terminal-read_str8 : _int _int _pointer _int -> _int)
(defrotk terminal-read_str16 : _int _int _pointer _int -> _int -> _int)
(defrotk terminal-read_str32 : _int _int _pointer _int -> _int -> _int)
(defrotk terminal-peek : -> _int)
(defrotk terminal-delay : _int -> _void)
(defrotk terminal-get8 : _pointer _pointer -> _pointer)
(defrotk terminal-get16 : _pointer _pointer -> _pointer)
(defrotk terminal-get32 : _pointer _pointer -> _pointer)

;color_t color_from_name8(const int8_t* name);
;color_t color_from_name16(const int16_t* name);
;color_t color_from_name32(const int32_t* name);

