;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex204) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
; A Nested-string is one of:
; – String
; – (make-layer Nested-string)

(make-layer (make-layer (make-layer "foo")))

; A Nested-number is one of:
; – Number
; – (make-layer Nested-number)

(make-layer (make-layer (make-layer 10)))

; A [Nested-ITEM] is one of:
; - ITEM
; - (make-layer ITEM)