;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname rev) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "htdp")))))
(define-struct editor (pre post))
; An Editor is (make-editor Lo1S Lo1S)
; An Lo1S is one of:
; – empty
; – (cons 1String Lo1S)
; interp. (string-append (implode (rev pre)) (implode post))
; is the text in the editor with the cursor between pre and post

(define good
  (cons "g" (cons "o" (cons "o" (cons "d" empty)))))
(define all
  (cons "a" (cons "l" (cons "l" empty))))
(define lla
  (cons "l" (cons "l" (cons "a" empty))))
 
; data example 1:
(make-editor all good)
 
; data example 2:
(make-editor lla good)

; rev: Lo1s -> Lo1s
; reverse a list

(check-expect
  (rev (cons "a" (cons "b" (cons "c" empty))))
  (cons "c" (cons "b" (cons "a" empty))))

(define (rev l)
  (cond
    [(empty? l) empty]
    [else (add-to-end (rev (rest l)) (first l))]))

; add-to-end: Lo1s String -> Lo1S
; add a character to the end of a list

(check-expect (add-to-end empty "a")
              (cons "a" empty))
(check-expect (add-to-end (cons "c" (cons "b" empty)) "a")
              (cons "c" (cons "b" (cons "a" empty))))

(define (add-to-end l s)
  (cond 
    [(empty? l) (cons s empty)]
    [else (cons (first l) ; Char
          (add-to-end (rest l) s)) ; Lo1s
          ]))