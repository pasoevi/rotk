#lang racket

(require ffi/unsafe)
(require ffi/unsafe/define)
(provide rotk defrotk)

(define rotk
  (ffi-lib (case (system-type 'os)
             ['windows "BearLibTerminal.dll"]
             [else "libBearLibTerminal.so"])))

;;(define-ffi-definer defroguet roguet)

(define-syntax defrotk
  (syntax-rules (:)
    [(_ name : type ...)
     (define name (get-ffi-obj
                   (regexp-replaces (symbol->string 'name) '((#rx"-" "_")
                                            (#rx"[+*?!]" "")
                                            (#rx"^" "TCOD_")))
                   rotk (_fun type ...)))]))





