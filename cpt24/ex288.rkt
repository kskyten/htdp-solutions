;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex288) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
(define-struct add (left right))
(define-struct mul (left right))

(define-struct fun (name argument))
; A function application (FA) is (make-fun String BSLF)

; A BSL-fun-expr is one of:
; - Number
; - Symbol
; - (make-add BSLF BSLF)
; - (make-mul BSLF BSLF)
; - (make-fun BSLF BSLF)

(define bslf0 (make-fun 'k
                        (make-add 1 1)))

(define bslf1 (make-mul 5
                        (make-fun 'k
                                  (make-add 1 1))))

(define bslf2 (make-mul (make-fun 'i 5)
                        (make-fun 'k
                                  (make-add 1 1))))