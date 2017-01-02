;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex2822) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "draw.rkt" "teachpack" "htdp")))))
; A Row is [List-of Boolean]
; True indicates that the position is available for the
; position of a queen. False indicates that the place is occupied
; or threatened.

; A Board is [List-of Row]

(define b0 (list
            (list true false false)
            (list false true false)
            (list false false true)))

; build-board: N (N N -> Boolean) -> Board
; Create a board of size nxn where each
; position (i, j) if fills with (f i j)

(check-expect (build-board 0 (lambda (i j) false)) empty)
(check-expect (build-board 2 (lambda (i j) false))
              '((#f #f)
                (#f #f)))

(define (build-board n f)
  (build-list n (lambda (i)
                  (build-list n
                              (lambda (j) (f i j))))))

; board-ref : board N N  ->  boolean
;; to access a position with indices i, j on a-board
;; Indexing starts from zero

(check-expect (board-ref b0 0 0) true)
(check-expect (board-ref b0 0 1) false)
(check-expect (board-ref b0 2 1) false)

(define (board-ref a-board i j)
  (list-ref (list-ref a-board i) j))