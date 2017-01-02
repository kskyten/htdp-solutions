;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex224) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
; find-name: String [String] -> Boolean
; Determine whether any of the names in lst
; is equal to or an extension to nm

(check-expect (find-name "John" (list "Johnno" "Peter"))
              true)
(check-expect (find-name "John" (list "Mary" "Peter"))
              false)

(define (find-name nm lon)
    (ormap (lambda (x) (string-contains? nm x)) lon))

; starts-with-a?: [String] -> Boolean
; Do all the names start with "a"?

(check-expect (starts-with-a? '("aapo" "aabraham"))
              true)
(check-expect (starts-with-a? '("uolevi" "aabraham"))
              false)

(define (starts-with-a? los)
  (andmap (lambda (name)
            (string=? "a" (string-ith name 0)))
          los))