;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex130) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
; List-of-temperatures -> Number
; compute the average temperature

(check-expect (average (cons 1 (cons 2 (cons 3 empty)))) 2)

(define (average alot)
  (/ (sum alot)
     (how-many alot)))

; List-of-temperatures -> Number
; add up the temperatures on the given list

(check-expect (sum (cons 1
                         (cons 2
                               (cons 3 empty))))
              6)

(define (sum alot)
  (cond
    [(empty? alot) 0]
    [else (+ (first alot) (sum (rest alot)))]))

; List-of-temperatures -> Number
; count the temperatures on the given list

(check-expect (how-many (cons 1
                         (cons 2
                               (cons 3 empty))))
              3)

(define (how-many alot)
  (cond
    [(empty? alot) 0]
    [(cons? alot) (+ 1 (how-many (rest alot)))]))

; checked-average: List-of-temperatures -> Number
; Checked version of average

(check-error (checked-average empty) "Can't compute average of an empty list.")
(check-expect (checked-average (cons 1 (cons 2 (cons 3 empty)))) 2)

(define (checked-average lst)
  (cond
    [(empty? lst) (error "Can't compute average of an empty list.")]
    [(cons? lst) (average lst)]
    [else (error "Expected a list, given: " lst)]))