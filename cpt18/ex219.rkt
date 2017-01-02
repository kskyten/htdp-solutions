;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex219) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
; Lon -> Lon
; constructs a list from the items in l in descending or
; ascending order

(check-expect (sort-a > empty) empty)
(check-expect (sort-a > '(1 3 4 2 9))
              '(9 4 3 2 1))

(define (sort-a R l0)
  (local (; Lon -> Lon
          (define (sort l)
            (cond
              [(empty? l) empty]
              [else (insert (first l) (sort (rest l)))]))
          
          ; Number Lon -> Lon
          ; Insert a number into a sorted list
          (define (insert an l)
            (cond
              [(empty? l) (list an)]
              [else
               (cond
                 [(R an (first l)) (cons an l)]
                 [else (cons (first l) (insert an (rest l)))])])))
    (sort l0)))


; sort->: LoN -> LoN
; Sort a list of number in descending order

(check-expect (sort-> empty) empty)
(check-expect (sort-> '(1 3 4 2 9))
              '(9 4 3 2 1))

(define (sort-> l)
  (sort-a > l))


; sort-<: LoN -> LoN
; Sort a list of number in ascending order

(check-expect (sort-< empty) empty)
(check-expect (sort-< '(1 3 4 2 9))
              '(1 2 3 4 9))

(define (sort-< l)
  (sort-a < l))