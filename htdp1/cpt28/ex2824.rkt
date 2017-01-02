;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex2824) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "draw.rkt" "teachpack" "htdp")))))
; A Row is [List-of Boolean]
; True indicates that the position is available for the
; position of a queen. False indicates that the place is occupied
; or threatened.

; A Board is [List-of Row]

(define b0 (list
            (list true false false)
            (list false true false)
            (list false false true)))

; Empty 3x3 board
(define mt3 (list (list true true true)
                  (list true true true)
                  (list true true true)))

; build-board: N (N N -> Boolean) -> Board
; Create a board of size nxn where each
; position (i, j) if fills with (f i j)

(check-expect (build-board 0 (lambda (i j) false)) empty)
(check-expect (build-board 2 (lambda (i j) false))
              '((#f #f)
                (#f #f)))

(define (build-board n f)
  (build-list n (lambda (i)
                  (build-list n
                              (lambda (j) (f i j))))))

; board-ref : board N N  ->  boolean
;; to access a position with indices i, j on a-board
;; Indexing starts from zero

(check-expect (board-ref b0 0 0) true)
(check-expect (board-ref b0 0 1) false)
(check-expect (board-ref b0 2 1) false)

(define (board-ref a-board i j)
  (list-ref (list-ref a-board i) j))

; threatened?; Posn Posn -> Boolean
; Does the queen in pos1 threaten pos2?

(check-expect (threatened? (make-posn 0 0) (make-posn 0 2)) true)
(check-expect (threatened? (make-posn 0 0) (make-posn 2 2)) true)
(check-expect (threatened? (make-posn 0 0) (make-posn 1 2)) false)

(define (threatened? pos1 pos2)
  (local (; slope: -> Number
          (define (slope)
            (/ (- (posn-y pos1) (posn-y pos2))
               (- (posn-x pos1) (posn-x pos2)))))
    (cond
      [(or (= (posn-x pos1) (posn-x pos2))
           (= (posn-y pos1) (posn-y pos2))) true]
      [(or (= (slope) 1)
           (= (slope) -1)) true]
      [else false])))

; placement: N Board -> Maybe Board
; Place n queens on a-board if it is possible.
; Produce false otherwise.

(check-expect (placement 1 (build-board 1 (lambda (i j) true)))
              (list (list false)))
(check-expect (placement 1 (build-board 1 (lambda (i j) false)))
              false)
(check-expect (placement 1 mt3)
              (list (list false false false)
                    (list false false true)
                    (list false true false)))
(check-expect (placement 1 (list (list false false false)
                                 (list false false true)
                                 (list false true false)))
              (list (list false false false)
                    (list false false false)
                    (list false false false)))
(check-expect (placement 2 mt3)
              (list (list false false false)
                    (list false false false)
                    (list false false false)))

(define (placement n a-board)
  (cond
    [(= 0 n) a-board]
    [(all-threatened? a-board) false]
    [else (local 
            (
             (define available (get-available a-board))
             )
            (cond
              [(empty? available) ...]
              [else (local (
                            (define potential-board (placement (- n 1) (place-queen (first available))))
                            )
                      (cond
                        [(boolean? potential-board) ...]
                        [else (placement (- n 1) (place-queen (rest available)))]))
               (placement (- n 1) )]))
     ; get a list of available positions
     ; try first position
     ; 
     (combine (subproblem )
              (subproblem ))]))

; place-to-available: [List-of Index] Board -> 

; place-queen: Index Board -> Board
; Place a queen on the board.

; get-available: Board -> [List-of Index]
; Get a list of available positions for the queens

; all-threatened?: Board -> Boolean
; Determine if all the positions are taken or threatened