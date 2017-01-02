;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex190) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
; FSM-State is a String that specifies a color. 

; for example:
(define RED "red")
(define GREEN "green")
(define YELLOW "yellow")

(define-struct transition (current next))
; A Transition is
;   (make-transition FSM-State FSM-State)

; A Transition* is
;   – empty
;   – (cons Transition Transition*)

(define t0
  (list (make-transition "red" "green")
        (make-transition "green" "yellow")
        (make-transition "yellow" "red")))

(define-struct fsm (current transitions))
; A FSM is (make-fsm FSM-State Transition*)

(define fsm0 (make-fsm "red" t0))

; state?: Any -> Boolean
; Is f a FSM-State?

(check-expect (state? RED) true)
(check-expect (state? 3) false)

(define (state? f)
  (string? f))

; state=?: Any Any -> Boolean
; Are the fsm-states the same?

(check-expect (state=? RED RED) true)
(check-expect (state=? RED GREEN) false)
(check-error (state=? "foo" 3) "state=?: FSM-State expected")

(define (state=? f1 f2)
  (cond
    [(and (state? f1)
          (state? f2)
          (string=? f1 f2)) true]
    [(and (state? f1)
          (state? f2)) false]
    [else (error "state=?: FSM-State expected")]))