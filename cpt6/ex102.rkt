;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex102) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
(define-struct vec (x y))
; A vec (short for velocity vector) is
; (make-vec PositiveNumber PositiveNumber)

; checked-make-vec: Any Any -> Vec
; Checked version of make-vec
(define (checked-make-vec x y)
  (cond
    [(and (positive? x)
          (positive? y))
     (make-vec x y)]
    [else (error "make-vec: positive numbers expected. Given: " x ", " y)]
    ))