;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex162) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "htdp")))))
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

; create-editor: String String -> Editor
; Create an editor from pre and post strings

(check-expect (create-editor "" "")
              (make-editor empty empty))
(check-expect (create-editor "all" "good")
              (make-editor lla good))

(define (create-editor pre post)
  (make-editor (reverse (explode pre)) (explode post)))