;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex133) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
; NEList-of-boolens is one of:
; - (cons Boolean empty)
; - (cons Boolean NEList-of-booleans)
; interp. a non-empty list of booleans

; all-true?: NEList-of-booleans -> Boolean
; Are all the values is lst true?

(check-expect (all-true? (cons true empty)) true)
(check-expect (all-true? (cons true
                               (cons true
                                     (cons true empty)))) true)
(check-expect (all-true? (cons true
                               (cons false
                                     (cons true empty)))) false)

(define (all-true? lst)
  (cond
    [(empty? (rest lst)) (boolean=? true (first lst))]
    [(cons? (rest lst)) (and (boolean=? true (first lst))
                             (all-true? (rest lst)))]
    [else (error "Expected non-empty list, given: " lst)]))


; one-true?: NEList-of-booleans -> Boolean
; Is there at least one true value in lst?

(check-expect (one-true? (cons true empty)) true)
(check-expect (one-true? (cons true
                               (cons true
                                     (cons true empty)))) true)
(check-expect (one-true? (cons true
                               (cons false
                                     (cons true empty)))) true)
(check-expect (one-true? (cons false
                               (cons false
                                     (cons false empty)))) false)

(define (one-true? lst)
  (cond
    [(empty? (rest lst)) (boolean=? true (first lst))]
    [(cons? (rest lst)) (or (boolean=? true (first lst))
                             (all-true? (rest lst)))]
    [else (error "Expected non-empty list, given: " lst)]))