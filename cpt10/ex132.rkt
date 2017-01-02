;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex132) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
; NEList-of-temperatures is one of:
; - (cons CTemperature empty)
; - (cons CTemperature NEList-of-temperatures)
; interp. a non-empty list of temperatures

; A CTemperature is a Number greater or equal to -256.

; NEList-of-temperatures -> Number
; compute the average temperature
 
(check-expect (average (cons 1 (cons 2 (cons 3 empty)))) 2)
 
(define (average anelot)
  (/ (sum anelot)
     (how-many anelot)))

; NEList-of-temperatures -> Number
; compute the sum of the given temperatures

(check-expect (sum (cons 1 (cons 2 (cons 3 empty)))) 6)

(define (sum anelot)
  (cond
    [(empty? (rest anelot)) (first anelot)]
    [(cons? (rest anelot)) (+ (first anelot) (sum (rest anelot)))]))

; how-many: NEList-of-temperatures -> Number
; Count the number of temperatures in lst

(check-expect (how-many (cons 24 empty)) 1)
(check-expect (how-many (cons 24
                              (cons 30 empty))) 2)

(define (how-many lst)
  (cond
    [(empty? (rest lst)) 1]
    [(cons? (rest lst))
     (+ 1 (how-many (rest lst)))]
    [else (error "Expected non-empty list, given: " lst)]))