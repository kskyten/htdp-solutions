;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex197) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
; List-of-numbers -> ... Nested List ...
; create a row for an HTML table from a list of numbers
(define (make-row l)
  (cond
    [(empty? l) '()]
    [else (cons (make-cell (first l)) (make-row (rest l)))]))
 
; X -> ... Nested List ...
; create a cell for an HTML table

(check-expect (make-cell 1)
              '(td 1))
(check-expect (make-cell "foo")
              '(td "foo"))

(define (make-cell n)
  `(td ,n))

;; List-of-numbers List-of-numbers -> ... Nested List ...
;; create an HTML table from two lists of numbers
;(define (make-table row1 row2)
;  `(table ((border "1"))
;          (tr ,@(make-row row1))
;          (tr ,@(make-row row2))))

; ranking: LoS -> LLoNS
; Append ranking numbers to a list of strings
(define (ranking los)
  (reverse (add-ranks (reverse los))))
 
; add-ranks: LoS -> LLoNS
; Append a number toan entry based on it's position
; in the list
(define (add-ranks los)
  (cond
    [(empty? los) '()]
    [else (cons (list (length los) (first los))
                (add-ranks (rest los)))]))

; make-ranking: LoS -> Table
; Make a table of rankings

(check-expect (make-ranking empty) '(table ((border "1"))))
(check-expect (make-ranking '("foo")) 
              '(table ((border "1"))
                      (tr (td 1) (td "foo"))))
(check-expect (make-ranking '("foo" "bar")) 
              '(table ((border "1"))
                      (tr (td 1) (td "foo"))
                      (tr (td 2) (td "bar"))))

(define (make-ranking los)
  (make-table (reverse (ranking los))))

; make-table: LLoNS -> Table
; Make a table from a list of ranking entries

(check-expect (make-table '((2 "bar")
                            (1 "foo")))
              '(table ((border "1"))
                      (tr (td 1) (td "foo"))
                      (tr (td 2) (td "bar"))))

(define (make-table llons)
  (cond
    [(empty? llons) '(table ((border "1")))]
    [else (append (make-table (rest llons))
                  `((tr ,@(make-row (first llons)))))]))