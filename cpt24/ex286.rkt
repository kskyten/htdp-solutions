;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex286) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
; An AL (association list) is [List-of Association].
; An Association is (cons Symbol (cons Number empty)).

; lookup-con: Symbol AL -> Number
; Looks up the first value assigned to variable var in the association list a

(check-expect (lookup-con 'x '((x 5))) 5)
(check-error  (lookup-con 'x '((s 4)) )
              "Undefined variable: 'x")

(define (lookup-con var al)
  (local (
          ; match?: Symbol Association -> Boolean
          ; Is the variable var defined in the association a
          (define (match? var a)
            (symbol=? var (first a)))
          )
    (cond
      [(empty? al) (error "Undefined variable: " var)]
      [else (if (match? var (first al))
                (second (first al))
                (lookup-con var (rest al)))])))


