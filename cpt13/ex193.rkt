;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex193) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
; FSM-State is a String that specifies a color.
(define RED "red")
(define GREEN "green")
(define YELLOW "yellow")

(define-struct transition (ke current next))
; A Transition is (make-transition KeyEvent FSM-State FSM-State)

; A Transition* is one of:
; - empty
; - (cons Transition Transition*)

(define t0
  (list (make-transition "g" "red" "green")
        (make-transition "g" "yellow" "green")
        (make-transition "g" "green" "green")
        (make-transition "y" "red" "yellow")
        (make-transition "y" "yellow" "yellow")
        (make-transition "y" "green" "yellow")
        (make-transition "r" "red" "red")
        (make-transition "r" "yellow" "red")
        (make-transition "r" "green" "red")))

(define-struct fsm (current transitions))
; A FSM is (make-fsm FSM-State Transition*)

(define fsm0 (make-fsm "red" t0))

; ---------- Functions -----------------

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

(check-expect (find-next-state fsm0 "g") (make-fsm "green" t0))
(check-expect (find-next-state fsm0 "r") (make-fsm "red" t0))
(check-expect (find-next-state fsm0 "y") (make-fsm "yellow" t0))
(check-expect (find-next-state fsm0 "q") (make-fsm "red" t0))

(define (find-next-state fsm ke)
  (make-fsm (find-ke ke (fsm-current fsm) (fsm-transitions fsm))
            (fsm-transitions fsm)))

; find-ke: KeyEvent FSM-State Transition* -> FSM-State
; Find the next state given the key event ke

(check-expect (find-ke "g" "red" t0) "green")
(check-expect (find-ke "y" "red" t0) "yellow")
(check-expect (find-ke "r" "red" t0) "red")
(check-expect (find-ke "d" "red" t0) "red")

(define (find-ke ke current trans)
  (cond
    [(empty? trans) current]
    [else (if (match? ke current (first trans))
              (transition-next (first trans))
              (find-ke ke current (rest trans)))]))

; match?: KeyEvent FSM-State Transition -> Boolean

(check-expect (match? "r" "green" (make-transition "r" "green" "yellow"))
              true)
(check-expect (match? "t" "green" (make-transition "r" "green" "yellow"))
              false)

(define (match? ke state trans)
  (and (key=? ke (transition-ke trans))
       (state=? state (transition-current trans))))

; FSM -> State
; interpret a given FSM, reacting to each key event with a transition
(define (simulate fsm0)
  (big-bang fsm0
            [to-draw state-as-colored-square]
            [on-key find-next-state]))