;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex125) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
; A List-of-amounts is one of:
; – empty
; – (cons PositiveNumber List-of-amounts)
; interp. a List-of-amounts represents some amounts of money 

; example list
(define l1 (cons 16
                 (cons 45
                       (cons 67 empty))))

; sum: List-of-amounts -> Number
; Compute the sum of amounts in l

(check-expect (sum empty) 0)
(check-expect (sum (cons 1
                         (cons 2
                               (cons 3 empty)))) 6)
(check-expect (sum l1) 128)


(define (sum l)
  (cond
    [(empty? l) 0]
    [(cons? l) (+ (first l) (sum (rest l)))]
    ))