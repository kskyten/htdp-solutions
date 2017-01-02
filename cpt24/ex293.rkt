;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex293) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
(define WRONG "wrong kind of S-expression")

(define-struct add (left right))
(define-struct mul (left right))

(define-struct fun (name arg))
; A function application (FA) is (make-fun String BSLF)

(define-struct fd (name param body))
; A BSL-fun-def (FD) is (make-fd Symbol Symbol BSLF)

; atom?; Any -> Boolean
; Is at an atom?

(check-expect (atom? 6) true)
(check-expect (atom? "foo") true)
(check-expect (atom? 'f) true)
(check-expect (atom? sqr) false)

(define (atom? at)
  (or (number? at)
      (string? at)
      (symbol? at)))

; parse: S-expr -> BSL-expr
; create representation of a BSL expression for s (if possible)

(check-error (parse '(f))
             WRONG)
(check-error (parse '(^ 1 2))
             WRONG)
(check-error (parse '(+ 1 2 3))
             WRONG)
(check-error (parse '(f "bar"))
             "strings not allowed")
(check-expect (parse '(f 1))
              (make-fun 'f 1))
(check-expect (parse '(f (+ 1 1)))
              (make-fun 'f
                        (make-add 1 1)))
(check-expect (parse '(f (+ 1 (* 2 3))))
              (make-fun 'f
                        (make-add 1 (make-mul 2 3))))

(define (parse s)
  (local (; S-expr -> BSL-expr
          (define (parse s)
            (cond
              [(atom? s) (parse-atom s)]
              [else (parse-sl s)]))
          
          ; SL -> BSL-expr
          (define (parse-sl s)
            (local ((define L (length s)))
              (cond
                [(< L 2)
                 (error WRONG)]
                [(= L 2)
                 (make-fun (first s) (parse (second s)))]
                [(and (= L 3) (symbol? (first s)))
                 (cond
                   [(symbol=? (first s) '+)
                    (make-add (parse (second s)) (parse (third s)))]
                   [(symbol=? (first s) '*)
                    (make-mul (parse (second s)) (parse (third s)))]
                   [else (error WRONG)])]
                [else
                 (error WRONG)])))
          
          ; Atom -> BSL-expr
          (define (parse-atom s)
            (cond
              [(number? s) s]
              [(string? s) (error "strings not allowed")]
              [(symbol? s) s])))
    (parse s)))

; def-parse: {S-expr} -> (tech "BSL-fun-def")
; create representation of a BSL definition for s (if possible)

(check-error (def-parse 5) WRONG)
(check-error (def-parse '(dfine (f x) (+ 1 x))) WRONG)
(check-error (def-parse '(define 4 (+ 1 x))) WRONG)
(check-error (def-parse '(define (f) (+ 1 x))) WRONG)
(check-error (def-parse '(define (f 4) (+ 1 x))) WRONG)
(check-expect (def-parse '(define (f x) (+ x 1)))
              (make-fd 'f 'x (make-add 'x 1)))
(check-expect (def-parse '(define (f x) (+ x (g 5))))
              (make-fd 'f 'x (make-add 'x (make-fun 'g 5))))

(define (def-parse s)
  (local (; S-expr -> BSL-fun-def
          (define (def-parse s)
            (cond
              [(atom? s) (error WRONG)]
              [else
               (if (and (= (length s) 3) (eq? (first s) 'define))
                   (head-parse (second s) (parse (third s)))
                   (error WRONG))]))
          ; S-expr BSL-expr -> BSL-fun-def
          (define (head-parse s body)
            (cond
              [(atom? s) (error WRONG)]
              [else
               (if (not (= (length s) 2))
                   (error WRONG)
                   (local ((define name (first s))
                           (define para (second s)))
                     (if (and (symbol? name) (symbol? para))
                         (make-fd name para body)
                         (error WRONG))))])))
    (def-parse s)))

; da-parse: SL -> FD*
; Parse a list of quoted function definitions

(check-expect (da-parse '((define (f x) (+ x 1))
                          (define (g x) (* x 2))))
              (list (make-fd 'f 'x (make-add 'x 1))
                    (make-fd 'g 'x (make-mul 'x 2))))

(define (da-parse sl)
  (map def-parse sl))