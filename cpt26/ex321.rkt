;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex321) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
; errors
(define EMPTY "Empty path given.")
(define NOPATH "No such path.")
(define INVALID "Invalid path.")

(define-struct branch (left right))
; A TOS is one of:
; – Symbol
; – (make-branch TOS TOS)

(define t0 (make-branch (make-branch 'a 'b)
                        (make-branch 'c 'd)))

; A Direction is one of:
; – 'left
; – 'right

; A list of Directions is also called a path. 

; tree-pick: TOS [Direction] -> Symbol
; Given a list of directions pick a node from the tree

(check-error (tree-pick 'a '(right right)) NOPATH)
(check-expect (tree-pick 'a '()) 'a)
(check-expect (tree-pick t0 '(right right)) 'd)
(check-expect (tree-pick t0 '(right left)) 'c)
(check-error (tree-pick t0 '()) EMPTY)
(check-error (tree-pick t0 '(left foo)) INVALID)

(define (tree-pick tos lod)
  (cond
    [(and (symbol? tos) (empty? lod)) tos]
    [(and (symbol? tos) (cons? lod)) (error NOPATH)]
    [(and (branch? tos) (empty? lod)) (error EMPTY)]
    [(and (branch? tos) (cons? lod))
     (cond
       [(symbol=? 'left (first lod))
        (tree-pick (branch-left tos) (rest lod))]
       [(symbol=? 'right (first lod))
        (tree-pick (branch-right tos) (rest lod))]
       [else (error INVALID)])]))