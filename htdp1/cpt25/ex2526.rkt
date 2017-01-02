;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex2526) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
;; general-quick-sort: (X X -> Boolean) [X] -> [X]
;; Sort a list using a comparator function

(check-expect (general-quick-sort < '())
              '())
(check-expect (general-quick-sort < '(8 3 8 2))
              '(2 3 8 8))

(define (general-quick-sort cmp alox)
  (local (
          ; smaller-items: [X] X -> [X]
          (define (smaller-items a-list item)
            (filter (lambda (x) (cmp x item)) a-list))
          
          ; larger-items: [X] X -> [X]
          (define (larger-items a-list item)
            (filter (lambda (x) (not (cmp x item))) a-list))
          )
  (cond
    [(empty? alox) empty]
    [(empty? (rest alox)) alox]
    [else (append (general-quick-sort cmp (smaller-items alox (first alox))) 
                  (list (first alox)) 
                  (general-quick-sort cmp (larger-items (rest alox) (first alox))))])))