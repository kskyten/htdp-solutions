;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex192) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
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

(define t1
  (list (make-transition "black" "white")
        (make-transition "white" "black")))

(define-struct fsm (current transitions))
; A FSM is (make-fsm FSM-State Transition*)

(define fsm0 (make-fsm "red" t0))

; BW Machine
(define bwm (make-fsm "black" t1))

;---------------- Functions -----------------

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

; FSM -> Image
; render current world state as a colored square

(check-expect (state-as-colored-square fsm0)
              (square 100 "solid" "red"))

(define (state-as-colored-square fsm)
  (square 100 "solid" (fsm-current fsm)))

; FSM KeyEvent -> FSM
; create an FSM that contains the next state, given that a key
; event took place

(check-expect (find-next-state fsm0 "n") (make-fsm "green" t0))
(check-expect (find-next-state fsm0 "q") (make-fsm "green" t0))
(check-expect (find-next-state fsm0 "y") (make-fsm "green" t0))

(define (find-next-state fsm ke)
  (make-fsm (find (fsm-transitions fsm) (fsm-current fsm))
            (fsm-transitions fsm)))

; Transition* FSM-State -> FSM-State
; find the state matching current in the transition table

(check-expect (find empty "red") "red")
(check-expect (find t0 "red") "green")
(check-expect (find t0 "green") "yellow")

(define (find transitions current)
  (cond
    [(empty? transitions) current]
    [else (if (state=? current (transition-current (first transitions)))
              (transition-next (first transitions))
              (find (rest transitions) current))]))

; FSM -> State
; interpret a given FSM, reacting to each key event with a transition
(define (simulate fsm0)
  (big-bang fsm0
            [to-draw state-as-colored-square]
            [on-key find-next-state]))