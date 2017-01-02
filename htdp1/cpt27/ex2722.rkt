;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex2722) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "draw.rkt" "teachpack" "htdp")))))
; A File is [List-of Symbol]

(define NEWLINE 'NL)

;; file->list-of-lines : file  ->  (listof (listof symbol))
;; to convert a file into a list of lines

(check-expect (file->list-of-lines empty) empty)
(check-expect (file->list-of-lines (list 'NL)) (list empty))
(check-expect (file->list-of-lines (list 'NL 'NL)) (list empty empty))
(check-expect (file->list-of-lines (list 'a 'b 'c 'NL 'd 'e 'NL 'f 'g 'h 'NL))
              (list (list 'a 'b 'c)
                    (list 'd 'e)
                    (list 'f 'g 'h)))

(define (file->list-of-lines afile)
  (local (
          ;; first-line : file  ->  (listof symbol)
          ;; to compute the prefix of afile up to the first occurrence of NEWLINE
          (define (first-line afile)
            (foldr (lambda (symbol los)
                     (if (newline? symbol)
                         empty
                         (cons symbol los)))
                   empty
                   afile))
          
          ;; remove-first-line : file  ->  (listof symbol)
          ;; to compute the suffix of afile behind the first occurrence of NEWLINE
          (define (remove-first-line afile)
            (cond
              [(empty? afile) empty]
              [else (cond
                      [(newline? (first afile)) (rest afile)]
                      [else (remove-first-line (rest afile))])]))
          
          ;; newline?: Symbol -> Boolean
          ; Determine if the given symbol is a newline
          (define (newline? symbol)
            (symbol=? symbol NEWLINE))
          )
    (cond
      [(empty? afile) empty]
      [else
       (cons (first-line afile)
             (file->list-of-lines (remove-first-line afile)))])))

