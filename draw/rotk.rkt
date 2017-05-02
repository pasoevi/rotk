#lang racket/gui

(define frame (new frame%
                   [label "Example"]
                   [width 300]
                   [height 300]))


(new canvas% [parent frame]
             [paint-callback
              (lambda (canvas dc)
                (send dc set-scale 3 3)
                (send dc set-text-foreground "blue")
                (send dc draw-text "D" 0 0)
                (send dc draw-rectangle
                      0 10   ; Top-left at (0, 10), 10 pixels down from top-left
                      30 10) ; 30 pixels wide and 10 pixels high
                (send dc draw-line
                      0 0    ; Start at (0, 0), the top-left corner
                      30 30) ; and draw to (30, 30), the bottom-right corner
                (send dc draw-line
                      0 30   ; Start at (0, 30), the bottom-left corner
                      30 0)  ; and draw to (30, 0), the top-right corner

                (send dc set-brush "white" 'transparent)
 
                (send dc draw-rectangle 0 0 100 30)
                (send dc draw-text "H" 5 1))])
 
(send frame show #t)

(define target (make-bitmap 650 480)) ; A 30x30 bitmap
;;(define dc (new bitmap-dc% [bitmap target]))



;; (send target save-file "box.png" 'png)
