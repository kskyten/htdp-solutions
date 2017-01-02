;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex325) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
(define LETTERS (explode "abcdefghijklmnopqrstuvwxyz"))
 
; A Letter is member? of LETTERS.
; A HMLetter is one of:
; - "_"
; - Letter

; A HMWord is [List-of HMLetter]
(define w0 (explode "banana"))
(define g0 (explode "b_____"))

; reveal: HMWord HMWord Letter -> HMWord
; Replace the underlines in the guess with the proposed letter
; if the letter appears in the word

(check-expect (reveal w0 g0 "a") (explode "ba_a_a"))
(check-expect (reveal w0 g0 "e") g0)

(define (reveal the-word the-guess letter)
  (cond
    [(empty? the-word) empty]
    [else (if (string=? (first the-word) letter)
              (cons (first the-word)
                    (reveal (rest the-word)
                            (rest the-guess)
                            letter))
              (cons (first the-guess)
                    (reveal (rest the-word)
                            (rest the-guess)
                            letter)))]))

; String -> String
; run a simplistic Hangman game where s is the word to be guessed,
; returns the guessed word
(define (main s)
  (local ((define the-word (explode s))
          (define the-guess (make-list (length the-word) "_"))
 
          ; HMWord -> Image
          ; render the current word as an image
          (define (render-word w)
            (local ((define l
                      (map (lambda (l) (if (string? l) l "_")) w))
                    (define s (implode l)))
              (text s 33 "black")))
 
          ; HMWord KeyEvent -> HMWord
          ; if ke is a letter, update the status;
          ; otherwise, return the current status
          (define (checked-reveal status ke)
            (cond
              [(member? ke LETTERS) (reveal the-word status ke)]
              [else status])))
    ; – IN –
    (implode
      (big-bang the-guess
        [to-draw render-word]
        [on-key  checked-reveal]))))