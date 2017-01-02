;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex330) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
;sexp=?: S-expr S-expr -> Boolean
; Are the two s-expressions are equal?

(check-expect (sexp=? 'a 'a) true)
(check-expect (sexp=? 1 1) true)
(check-expect (sexp=? "s" "a") false)

(check-expect (sexp=? 'a '()) false)
(check-expect (sexp=? 'a '(a b)) false)
(check-expect (sexp=? '() 'a) false)
(check-expect (sexp=? '() '()) true)
(check-expect (sexp=? '() '(a b)) false)

(check-expect (sexp=? '(a b) 'a) false)
(check-expect (sexp=? '(a b) '()) false)
(check-expect (sexp=? '(a b) '(b c)) false)
(check-expect (sexp=? '(a b) '(a b)) true)

(define (sexp=? sexp1 sexp2)
  (cond
    [(and (empty? sexp1) (empty? sexp2)) true]
    [(and (atom? sexp1) (atom? sexp2)) (equal? sexp1 sexp2)]
    [(and (cons? sexp1) (cons? sexp2))
     (and (sexp=? (first sexp1) (first sexp2))
          (sexp=? (rest sexp1) (rest sexp2)))]
    [else false]))

; atom?: Any -> Boolean
; Is at an atom?
(define (atom? at)
  (or (number? at)
      (string? at)
      (symbol? at)))