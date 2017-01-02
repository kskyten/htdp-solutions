;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex147) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
; constants
(define RATE 12)

; List-of-numbers -> List-of-numbers
; compute the weekly wages for all given weekly hours
(check-expect (wage* empty) empty)
(check-expect (wage* (list 28))
              (list 336))
(check-expect (wage* (list 40 28))
              (list 480 336))
(check-error (wage* (list 10 20 140))
             "Input error: too many working hours in some entry")

(define (wage* alon)
  (cond
    [(empty? alon) empty]
    [else (if (< (first alon) 100)
              (cons (wage (first alon)) (wage* (rest alon)))
              (error "Input error: too many working hours in some entry"))]))
 
; Number -> Number
; compute the wage for h hours of work
(define (wage h)
  (* RATE h))