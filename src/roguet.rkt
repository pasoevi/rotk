#lang racket/base

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

(defroguet terminal-print8 : _int _int _string -> _int)
(defroguet terminal-print16 : _int _int _pointer -> _int)
(defroguet terminal-print32 : _int _int _pointer -> _int)
;(defroguet terminal-print : _int _int _pointer -> _int)

(defroguet terminal-read : -> _void)

;; Test

(terminal-open)
(terminal-set8 "window: size=32x8")
(terminal-set8 "font: VeraMoBd.ttf, size=12")
(terminal-print8 1 1 "Hello, World")
(terminal-refresh)
(terminal-read)
(terminal-close)


;void terminal_color(color_t color);
;void terminal_bkcolor(color_t color);
;void terminal_composition(int mode);
;void terminal_put(int x, int y, int code);
;void terminal_put_ext(int x, int y, int dx, int dy, int code, color_t* corners);
;int terminal_pick(int x, int y, int index);
;color_t terminal_pick_color(int x, int y, int index);
;color_t terminal_pick_bkcolor(int x, int y);
;int terminal_print8(int x, int y, const int8_t* s);
;int terminal_print16(int x, int y, const int16_t* s);
;int terminal_print32(int x, int y, const int32_t* s);
;int terminal_measure8(const int8_t* s);
;int terminal_measure16(const int16_t* s);
;int terminal_measure32(const int32_t* s);
;int terminal_has_input();
;int terminal_state(int code);
;int terminal_read();
;int terminal_read_str8(int x, int y, int8_t* buffer, int max);
;int terminal_read_str16(int x, int y, int16_t* buffer, int max);
;int terminal_read_str32(int x, int y, int32_t* buffer, int max);
;int terminal_peek();
;void terminal_delay(int period);
;const int8_t* terminal_get8(const int8_t* key, const int8_t* default_);
;const int16_t* terminal_get16(const int16_t* key, const int16_t* default_);
;const int32_t* terminal_get32(const int32_t* key, const int32_t* default_);
;color_t color_from_name8(const int8_t* name);
;color_t color_from_name16(const int16_t* name);
;color_t color_from_name32(const int32_t* name);
