;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex64) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
(define-struct phone (area number))
; phone is a structure: (make-phone Number String)
; interp. a phone number

(define-struct phone# (area switch phone))
; phone# is a structure: (make-phone# Number Number Number)
; interp. a phone number where:
; - area and switch are in the interval [0,999]
; - phone is in the interval [0,9999]