;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex126) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
; A List-of-numbers is one of:
; – empty
; – (cons Number List-of-numbers)

; pos?: List-of-numbers -> Boolean
; Are all the numbers in l positive?

(check-expect (pos? empty) true)
(check-expect (pos? (cons 3 empty)) true)
(check-expect (pos? (cons 3
                          (cons 5
                                (cons 8 empty)))) true)
(check-expect (pos? (cons 3
                          (cons -5
                                (cons 8 empty)))) false)

(define (pos? l)
  (cond
    [(empty? l) true]
    [(cons? l) (and (positive? (first l))
                    (pos? (rest l)))]
    ))