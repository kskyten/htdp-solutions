;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex161) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
; A Matrix is one of:
;  – empty
;  – (cons LN Matrix)
 
; An LN is one of:
;  – empty
;  – (cons Number LN)
 
; interp. a matrix is a list of rows, a row is a list of numbers
; constraint: all rows are of the same length

; example data
(define row1 (cons 11 (cons 12 empty)))
(define row2 (cons 21 (cons 22 empty)))
(define mat1 (cons row1 (cons row2 empty)))

; Matrix -> Matrix
; transpose the items on the given matrix along the diagonal
 
(define wor1 (cons 11 (cons 21 empty)))
(define wor2 (cons 12 (cons 22 empty)))
(define tam1 (cons wor1 (cons wor2 empty)))

(check-expect (transpose mat1) tam1)
 
(define (transpose lln)
  (cond
    [(empty? (first lln)) empty]
    [else (cons (first* lln) (transpose (rest* lln)))]))

; first*: Matrix