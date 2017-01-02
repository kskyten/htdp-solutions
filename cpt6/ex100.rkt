;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex100) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
(require 2htdp/image)

; constants
(define SIDE 100)
(define MT (empty-scene SIDE SIDE))
(define ACCEPTING (square SIDE "solid" "yellow"))
(define ERROR (square SIDE "solid" "red"))
(define FINISH (square SIDE "solid" "green"))

; ETS (ExpectsToSee) is one of:
; – AA
; – BC
; – DD
; – ER

(define AA "start, expect to see an 'a' next")
(define BC "expect to see: 'b', 'c', or 'd'")
(define DD "encountered a 'd', finished")
(define ER "error, user pressed illegal key")

; render: ETS -> Image
; render the state of the program

(check-expect (render AA) MT)
(check-expect (render BC) ACCEPTING)
(check-expect (render DD) FINISH)
(check-expect (render ER) ERROR)

(define (render ets)
  (cond
    [(string=? ets AA) MT]
    [(string=? ets BC) ACCEPTING]
    [(string=? ets DD) FINISH]
    [(string=? ets ER) ERROR]
    ))

; next: ETS KeyEvent -> ETS
; determine the next state given ke

(check-expect (next AA "a") BC)
(check-expect (next AA "d") DD)
(check-expect (next AA "b") ER)

(check-expect (next BC "b") BC)
(check-expect (next BC "c") BC)
(check-expect (next BC "d") DD)
(check-expect (next BC "e") ER)

(define (next ets ke)
  (cond
    [(string=? ets AA) (aa-ke ke)]
    [(string=? ets BC) (bc-ke ke)]
    [else ets]
    ))

; aa-ke: KeyEvent -> ETS
; Handle key event ke in the state AA

(check-expect (aa-ke "a") BC)
(check-expect (aa-ke "d") DD)
(check-expect (aa-ke "r") ER)

(define (aa-ke ke)
  (cond
    [(key=? ke "a") BC]
    [(key=? ke "d") DD]
    [else ER]
    ))

; bc-ke: KeyEvent -> ETS
; Handle key event ke in the state BC

(check-expect (bc-ke "b") BC)
(check-expect (bc-ke "c") BC)
(check-expect (bc-ke "d") DD)
(check-expect (bc-ke "t") ER)

(define (bc-ke ke)
  (cond
    [(key=? ke "b") BC]
    [(key=? ke "c") BC]
    [(key=? ke "d") DD]
    [else ER]
    ))

; end?: ETS -> Boolean
; Is there an error or is the key sequence finished?

(check-expect (end? AA) false)
(check-expect (end? BC) false)
(check-expect (end? DD) true)
(check-expect (end? ER) true)

(define (end? ets)
  (cond
    [(or (string=? ets DD)
         (string=? ets ER)) true]
    [else false]
    ))

; main: ETS -> ETS
; Initialize state and begin program
(define (main ets)
  (big-bang ets
            [on-key next]
            [to-draw render]
            [stop-when end?]
            ))