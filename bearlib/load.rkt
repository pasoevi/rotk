#lang racket

(require ffi/unsafe)
(require ffi/unsafe/define)
(provide roguet defroguet)

(define roguet
  (ffi-lib (case (system-type 'os)
             ['windows "BearLibTerminal.dll"]
             [else "BearLibTerminal.so"])))

;;(define-ffi-definer defroguet roguet)

(define-syntax defroguet
  (syntax-rules (:)
    [(_ name : type ...)
     (define name (get-ffi-obj
                   (regexp-replaces (symbol->string 'name) '((#rx"-" "_")
                                            (#rx"[+*?!]" "")
                                            (#rx"^" "TCOD_")))
                   roguet (_fun type ...)))]))





