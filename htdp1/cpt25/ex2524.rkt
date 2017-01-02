;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex2524) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
;; quick-sort : (listof number)  ->  (listof number)
;; to create a list of numbers with the same numbers as
;; alon sorted in ascending order

(check-expect (quick-sort '())
              '())
(check-expect (quick-sort '(8 3 8 2))
              '(2 3 8 8))
(check-expect (quick-sort '(1 9 2 8 3 7 4 6 5))
              '(1 2 3 4 5 6 7 8 9))
(check-expect (quick-sort '(1 9 2 8 3 7 4 6 5 10 20 11 19 12 18
                              13 17 14 16 15))
              '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17
                  18 19 20))

(define (quick-sort alon)
  (local (
          (define THRESHOLD 10)
          (define L (length alon))
          
          ; [Number] -> [Number](sorted)
          (define (quick-sort alon)
            (cond
              [(empty? alon) empty]
              [(empty? (rest alon)) alon]
              [else (append (quick-sort (smaller-items alon (first alon))) 
                            (list (first alon)) 
                            (quick-sort (larger-items (rest alon) (first alon))))]))
          )
    (cond
      [(> L THRESHOLD) (quick-sort alon)]
      [else (sort alon <)])))

;; larger-items : (listof number) number  ->  (listof number)
;; to create a list with all those numbers on alon  
;; that are larger than threshold
(define (larger-items alon threshold)
  (cond
    [(empty? alon) empty]
    [else (if (>= (first alon) threshold) 
              (cons (first alon) (larger-items (rest alon) threshold))
              (larger-items (rest alon) threshold))]))

;; smaller-items : (listof number) number  ->  (listof number)
;; to create a list with all those numbers on alon  
;; that are smaller than threshold
(define (smaller-items alon threshold)
  (cond
    [(empty? alon) empty]
    [else (if (< (first alon) threshold) 
              (cons (first alon) (smaller-items (rest alon) threshold))
              (smaller-items (rest alon) threshold))]))
