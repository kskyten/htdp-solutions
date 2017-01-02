;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex211) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
; (X Y -> Y) Y [List-of X] -> Y
; fold the list l from the right
; with initial value i with function f

(check-expect (fold1 + 0 '(1 2 3)) 6)
(check-expect (fold1 * 1 '(1 2 3)) 6)

(define (fold1 f i l)
  (cond
    [(empty? l) i]
    [else
     (f (first l) ; X
        (fold1 f i (rest l)))])) ; Y