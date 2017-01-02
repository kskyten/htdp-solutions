;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex123) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
; contains?: List-of-strings -> Boolean
; Does the list l contain the string s?

(check-expect (contains? "foo" empty) false)
(check-expect (contains? "foo" (cons "foo" empty)) true)

(check-expect (contains? "foo" (cons "bar" 
                                     (cons "baz" 
                                           (cons "guu" empty)))) false)

(check-expect (contains? "foo" (cons "bar" 
                                     (cons "baz" 
                                           (cons "foo" empty)))) true)

(define (contains? s l)
  (cond
    [(empty? l) false]
    [(cons? l)
     (or (string=? s (first l))
         (contains? s (rest l)))]
    ))