;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname los->string) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "htdp")))))
; line->string: LoS -> String
; Convert a list-of-strings into a string

;(check-expect (line->string empty) "\n")
(check-expect (line->string (list "This" "is" "a" "line")) "This is a line")

(define (line->string s)
  (cond
    [(empty? (rest s)) (first s)]
    [(cons? (rest s)) (string-append (first s) " " (line->string (rest s)))]))