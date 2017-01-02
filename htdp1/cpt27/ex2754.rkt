;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex2754) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "draw.rkt" "teachpack" "htdp")))))
; subtract: [Number] [Number] -> [Number]
; Substract the first list from the second to get 0 as the first
; number of the second list. Return rest of this list
; ASSUMPTION: The lists are of equal length

(check-expect (subtract '(1 2 3) '(0 1 2)) '(1 2))
(check-expect (subtract '(1 6 8) '(1 3 5)) '(-3 -3))
(check-expect (subtract '(2 3) '(-4 2)) '(8))

(define (subtract l1 l2)
  (local (; subtract-aux; Number [Number] [Number] -> [Number]
          ; Subtract l1 from l2 factor times
          ; ASSUMPTION: Lists are non-empty and equal length
          
          (define (subtract-aux factor l1 l2)
            (cond
              [(empty? l1) empty]
              [else (cons (- (first l2) (* factor (first l1)))
                          (subtract-aux factor (rest l1) (rest l2)))])))
    (cond
      [(empty? l1) empty]
      [(= 0 (first l2)) (rest l2)]
      [else
       (subtract-aux (/ (first l2) (first l1))
                     (rest l1) (rest l2))])))

; triangulate: [[Number]] -> [[Number]]
; Triangulate a matrix with gaussian elimination

(check-expect (triangulate '((1 2 3)))
              '((1 2 3)))
(check-expect (triangulate '((1 2)
                             (3 4)))
              (list '(1 2)
                    (subtract '(1 2) '(3 4))))

(check-expect (triangulate '((2 2 3 10)
                             (2 5 12 31)
                             (4 1 -2 1)))
              
              (list '(2 2 3 10)
                    (subtract '(2 2 3 10) '(2 5 12 31))
                    (subtract (subtract '(2 2 3 10) '(2 5 12 31))
                              (subtract '(2 2 3 10) '(4 1 -2 1)))))

(define (triangulate mat)
  (cond
    [(empty? mat) empty]
    [(= 1 (length mat)) mat]
    [else
     (cons (first mat)
           (triangulate (subtract-from-all (first mat) (rest mat))))]))

; subtract-from-all: [Number] [[Number]] -> [[Number]]
; Subtract the row from all the rows in mat

(check-expect (subtract-from-all '(2 2 3 10) '((2 5 12 31)
                                               (4 1 -2 1)))
              (list (subtract '(2 2 3 10) '(2 5 12 31))
                    (subtract '(2 2 3 10) '(4 1 -2 1))))

(define (subtract-from-all row mat)
  (map (lambda (row2) (subtract row row2)) mat))
