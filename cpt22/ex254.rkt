;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex254) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
; atom?: Any -> Boolean
; Is a an atom?

(check-expect (atom? 'foo) true)
(check-expect (atom? 4) true)
(check-expect (atom? "bar") true)
(check-expect (atom? sin) false)

(define (atom? a)
  (cond
    [(or (number? a)
         (string? a)
         (symbol? a)) true]
    [else false]))