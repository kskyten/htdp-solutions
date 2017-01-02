;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex191) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
; FSM-State is a String that specifies a color. 

; States for the BW Machine
(define STATES (list "black" "white"))

(define-struct transition (current next))
; A Transition is
;   (make-transition FSM-State FSM-State)

; A Transition* is
;   – empty
;   – (cons Transition Transition*)

(define t0
  (list (make-transition "black" "white")
        (make-transition "white" "black")))

(define-struct fsm (current transitions))
; A FSM is (make-fsm FSM-State Transition*)

; BW Machine
(define bw-machine (make-fsm "white" t0))

; state?: Any -> Boolean
; Is f a FSM-State?

(check-expect (state? "black") true)
(check-expect (state? 3) false)

(define (state? f)
  (member? f STATES))

; state=?: Any Any -> Boolean
; Are the fsm-states the same?

(check-expect (state=? "black" "black") true)
(check-expect (state=? "black" "white") false)
(check-error (state=? "foo" 3) "state=?: FSM-State expected")

(define (state=? f1 f2)
  (cond
    [(and (state? f1)
          (state? f2)
          (string=? f1 f2)) true]
    [(and (state? f1)
          (state? f2)) false]
    [else (error "state=?: FSM-State expected")]))