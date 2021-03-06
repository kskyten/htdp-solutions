;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex296) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
; errors
(define WRONG "wrong kind of S-expression")
(define UNDEFINEDV "undefined variable: ")
(define UNDEFINEDF "undefined function: ")

(define-struct add (left right))
(define-struct mul (left right))

(define-struct fun (name arg))
; A function application (FA) is (make-fun String BSLF)

(define-struct fd (name param body))
; A BSL-fun-def (FD) is (make-fd Symbol Symbol BSLF)

(define f (make-fd 'f 'x (make-add 3 'x)))
(define g (make-fd 'g 'y (make-fun 'f
                                   (make-mul 2 'y))))
(define h (make-fd 'h 'v (make-add (make-fun 'f 'v)
                                   (make-fun 'g 'v))))

; A BSL-fun-def* (FD*) is [List-of FD]
(define da-fgh (list f g h))

(define-struct con (name value))
; A constant definition (CD) is (make-con Symbol BSLF)

(define c0 (make-con 'foo (make-add 1 (make-mul 2 3))))
(define c1 (make-con 'hello (make-mul (make-fun 'f 2) 3)))

; A BSL-da-all (DA) is one of:
; - empty
; - (cons CD DA)
; - (cons FD DA)

(define da0 (list c1 c0 f g h))

; A BSL-fun-expr is one of:
; - Number
; - Symbol
; - (make-add BSLF BSLF)
; - (make-mul BSLF BSLF)
; - (make-fun BSLF BSLF)

(define bslf0 (make-fun 'k
                        (make-add 1 1)))

(define bslf1 (make-mul 5
                        (make-fun 'k
                                  (make-add 1 1))))

(define bslf2 (make-mul (make-fun 'i 5)
                        (make-fun 'k
                                  (make-add 1 1))))


; lookup-con-def: Symbol BSL-da-all -> BSLF
; Retrieve the definition of the constant f from da

(check-expect (lookup-con-def 'foo da0)
              (make-add 1 (make-mul 2 3)))
(check-error (lookup-con-def 'f da0)
             (string-append UNDEFINEDV "'f"))


(define (lookup-con-def f da)
  (cond
    [(empty? da) (error UNDEFINEDV f)]
    [(con? (first da))
     (if (symbol=? f (con-name (first da)))
         (con-value (first da))
         (lookup-con-def f (rest da)))]
    [(fd? (first da))
     (lookup-con-def f (rest da))]))


; lookup-fun-def: Symbol BSL-da-all -> BSLF
; Retrieve the definition for function f from da

(check-expect (lookup-fun-def 'f da0) f)
(check-error (lookup-fun-def 'k da0)
             (string-append UNDEFINEDF "'k"))

(define (lookup-fun-def f da)
  (cond
    [(empty? da) (error UNDEFINEDF f)]
    [(con? (first da))
     (lookup-fun-def f (rest da))]
    [(fd? (first da))
     (if (symbol=? f (fd-name (first da)))
         (first da)
         (lookup-fun-def f (rest da)))]))

; subst: BSLV Symbol Number -> BSLV
; Replace symbol sy for number n in BSL-expression bsl

(check-expect (subst (make-add 'x 3) 'x 5)
              (make-add 5 3))
(check-expect (subst (make-add 'x 3) 'y 5)
              (make-add 'x 3))
(check-expect (subst (make-add (make-mul 'x 5) 3) 'x 5)
              (make-add (make-mul 5 5) 3))

(define (subst bsl sy n)
  (cond
    [(number? bsl) bsl]
    [(symbol? bsl) (if (symbol=? sy bsl) n bsl)]
    [(add? bsl) (make-add (subst (add-left bsl) sy n) (subst (add-right bsl) sy n))]
    [(mul? bsl) (make-mul (subst (mul-left bsl) sy n) (subst (mul-right bsl) sy n))]))

; eval-definition1: BSLF Symbol Symbol BSLF
; Evaluate a BSL expression exp with a function application
; of the function f with the argument x. The function f is defined as
; the expression body

(check-error (eval-definition1 (make-add 1 'v) 'f 'x (make-add 'x 1))
             "Can not evaluate an expression with variables.")
(check-error (eval-definition1 (make-add 1 (make-fun 'g 1))
                               'f 'x (make-add 'x 1))
             "Can not evaluate an expression with variables.")
(check-expect (eval-definition1 (make-add 1 1) 'f 'x (make-add 'x 1))
              2)
(check-expect (eval-definition1 (make-add 1 (make-fun 'f 1))
                                'f 'x (make-add 'x 1))
              3)
(check-expect (eval-definition1 (make-mul 1 (make-fun 'f 1))
                                'f 'x (make-add 'x 1))
              2)


(define (eval-definition1 exp f x body)
  (cond
    [(number? exp) exp]
    [(symbol? exp) (error "Can not evaluate an expression with variables.")]
    [(add? exp) (+ (eval-definition1 (add-left exp) f x body)
                   (eval-definition1 (add-right exp) f x body))]
    [(mul? exp) (* (eval-definition1 (mul-left exp) f x body)
                   (eval-definition1 (mul-right exp) f x body))]
    [(fun? exp) (if (symbol=? f (fun-name exp))
                    (eval-definition1
                     (subst body x (eval-definition1 (fun-arg exp) f x body))
                     f
                     x
                     body)
                    (error "Can not evaluate an expression with variables."))]))

; eval-all: BSLF BSL-da-all -> Number
; Evaluate an expression e with definitions from da

(check-error (eval-all (make-add 1 'v) da0)
             (string-append UNDEFINEDV "'v"))
(check-expect (eval-all (make-add 1 1) da0) 2)
(check-expect (eval-all (make-add 1 'foo) da0) 8)
(check-expect (eval-all (make-add 1 (make-fun 'f 7)) da0) 11)
; Does not evaluate function calls with variables.

(define (eval-all e da)
  (cond
    [(number? e) e]
    [(symbol? e) (eval-all (lookup-con-def e da) da)]
    [(add? e) (+ (eval-all (add-left e) da)
                 (eval-all (add-right e) da))]
    [(mul? e) (* (eval-all (mul-left e) da)
                 (eval-all (mul-right e) da))]
    [(fun? e) (local (
                      (define result (lookup-fun-def (fun-name e) da))
                      ) 
                (eval-definition1 e
                                  (fd-name result)
                                  (fd-param result)
                                  (fd-body result)))]))

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
(check-expect (def-parse '(define foo (+ 2 3)))
              (make-con 'foo (make-add 2 3)))


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
              [(or (number? s)
                   (string? s))
               (error WRONG)]
              [(symbol? s) 
               (make-con s body)]
              [else
               (if (not (= (length s) 2))
                   (error WRONG)
                   (local ((define name (first s))
                           (define para (second s)))
                     (if (and (symbol? name) (symbol? para))
                         (make-fd name para body)
                         (error WRONG))))])))
    (def-parse s)))

; da-parse: SL -> BSL-da-all
; Parse a list of quoted function definitions

(check-expect (da-parse '((define (f x) (+ x 1))
                          (define (g x) (* x 2))
                          (define foo (+ 1 1))))
              (list (make-fd 'f 'x (make-add 'x 1))
                    (make-fd 'g 'x (make-mul 'x 2))
                    (make-con 'foo (make-add 1 1))))

(define (da-parse sl)
  (map def-parse sl))

; eval-all-sexpr: S-expr SL -> Number
; Evaluate the value of the expression representef by sexp
; with the definitions from sl

(check-expect (eval-all-sexpr '(+ 1 1) empty) 2)
(check-expect (eval-all-sexpr '(+ 1 foo) '((define foo 7))) 8)
(check-expect (eval-all-sexpr '(f 2) '((define (f x) (+ x 2)))) 4)

(define (eval-all-sexpr sexp sl)
  (eval-all (parse sexp) (da-parse sl)))