;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex72) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")))))
(require 2htdp/image)

; constants
(define WIDTH 200)
(define HEIGHT 20)
(define FONT-SIZE 11)
(define FONT-COLOR "black")

; graphical constants
(define MT (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 1 HEIGHT "solid" "red"))

(define-struct editor (pre post))
; Editor = (make-editor String String)
; interp. (make-editor s t) means the text in the editor is
; (string-append s t) with the cursor displayed between s and t

; test data
(define tst (make-editor "hello " "world"))

; render: Editor -> Image
; Render the editor state

(check-expect (render (make-editor "hello " "world"))
              (overlay/align
               "left" "center"
               (beside (render-text "hello ")
                       CURSOR
                       (render-text "world"))
               MT))

(define (render edt)
      (overlay/align
               "left" "center"
               (beside (render-text (editor-pre edt))
                       CURSOR
                       (render-text (editor-post edt)))
               MT))

; render-text: String -> Image
; Render the string str as an image

(check-expect (render-text "hello") (text "hello" FONT-SIZE FONT-COLOR))

(define (render-text str)
  (text str FONT-SIZE FONT-COLOR))