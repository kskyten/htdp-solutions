;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex164) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
; constants
(define HEIGHT 20) ; the height of the editor
(define WIDTH 200) ; its width
(define FONT-SIZE 11) ; the font size
(define FONT-COLOR "black") ; the font color

; graphical constants
(define MT (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 1 HEIGHT "solid" "red"))

(define-struct editor (pre post))
; An Editor is (make-editor Lo1S Lo1S)
; An Lo1S is one of:
; – empty
; – (cons 1String Lo1S)
; interp. (string-append (implode (rev pre)) (implode post))
; is the text in the editor with the cursor between pre and post

(define good
  (cons "g" (cons "o" (cons "o" (cons "d" empty)))))
(define lla
  (cons "l" (cons "l" (cons "a" empty))))

; data example:
(define EDT (make-editor lla good))

; Editor -> Image
; render an editor as an image of the two texts separated by the cursor

(check-expect (editor-render (create-editor "" ""))
              (place-image/align
               (beside (text "" FONT-SIZE FONT-COLOR)
                       CURSOR
                       (text "" FONT-SIZE FONT-COLOR))
               1 1
               "left" "top"
               MT))
(check-expect (editor-render EDT)
              (place-image/align
               (beside (text "all" FONT-SIZE FONT-COLOR)
                       CURSOR
                       (text "good" FONT-SIZE FONT-COLOR))
               1 1
               "left" "top"
               MT))

(define (editor-render ed)
  (place-image/align
   (beside (editor->text (reverse (editor-pre ed)))
          CURSOR
          (editor->text (editor-post ed)))
          1 1
          "left" "top"
          MT))

; editor->text: Lo1s -> Image
; render a list of 1Strings as a text image

(check-expect
  (editor->text (cons "p" (cons "o" (cons "s" (cons "t" empty)))))
  (text "post" FONT-SIZE FONT-COLOR))

(define (editor->text s)
  (text (implode s) FONT-SIZE FONT-COLOR))

; Editor KeyEvent -> Editor
; deal with a key event, given some editor

; adding text
(check-expect (editor-kh (create-editor "" "") "h") (create-editor "h" ""))
(check-expect (editor-kh (create-editor "hel" "o") "l") (create-editor "hell" "o"))
(check-expect (editor-kh (create-editor "" "ello") "h") (create-editor "h" "ello"))
(check-expect (editor-kh (create-editor "hello" "") " ") (create-editor "hello " ""))

; deleting text
(check-expect (editor-kh (create-editor "hello" "") "\b") (create-editor "hell" ""))
(check-expect (editor-kh (create-editor "hel" "lo") "\b") (create-editor "he" "lo"))
(check-expect (editor-kh (create-editor "" "hello") "\b") (create-editor "" "hello"))
(check-expect (editor-kh (create-editor "" "") "\b") (create-editor "" ""))

; ignored keys
(check-expect (editor-kh (create-editor "hello" "") "\t") (create-editor "hello" ""))
(check-expect (editor-kh (create-editor "hello" "") "shift") (create-editor "hello" ""))
(check-expect (editor-kh (create-editor "hel" "lo") "\u007F") (create-editor "hel" "lo"))

; movement
(check-expect (editor-kh (create-editor "" "") "left") (create-editor "" ""))
(check-expect (editor-kh (create-editor "hell" "") "left") (create-editor "hel" "l"))
(check-expect (editor-kh (create-editor "hel" "lo") "left") (create-editor "he" "llo"))
(check-expect (editor-kh (create-editor "" "hello") "left") (create-editor "" "hello"))

(check-expect (editor-kh (create-editor "" "") "right") (create-editor "" ""))
(check-expect (editor-kh (create-editor "" "hello") "right") (create-editor "h" "ello"))
(check-expect (editor-kh (create-editor "hel" "lo") "right") (create-editor "hell" "o"))
(check-expect (editor-kh (create-editor "hello" "") "right") (create-editor "hello" ""))

(define (editor-kh ed k)
  (cond
    [(key=? k "left") (editor-lft ed)]
    [(key=? k "right") (editor-rgt ed)]
    [(key=? k "\b") (editor-del ed)]
    [(key=? k "\t") ed]
    [(key=? k "\r") ed]
    [(key=? k "\u007F") ed]
    [(= (string-length k) 1) (editor-ins ed k)]
    [else ed]))

; editor-ins: Editor KeyEvent -> Editor
; insert the 1String k between pre and post

(check-expect
 (editor-ins (make-editor empty empty) "e")
 (make-editor (cons "e" empty) empty))

(check-expect
 (editor-ins (make-editor (cons "d" empty)
                          (cons "f" (cons "g" empty)))
             "e")
 (make-editor (cons "e" (cons "d" empty))
              (cons "f" (cons "g" empty))))

(define (editor-ins ed k)
  (make-editor (cons k (editor-pre ed)) (editor-post ed)))

; editor-lft: Editor -> Editor
; move the cursor position one 1String left, if possible

(check-expect (editor-lft (create-editor "" "")) (create-editor "" ""))
(check-expect (editor-lft (create-editor "hell" "")) (create-editor "hel" "l"))
(check-expect (editor-lft (create-editor "hel" "lo")) (create-editor "he" "llo"))
(check-expect (editor-lft (create-editor "" "hello")) (create-editor "" "hello"))

(define (editor-lft ed)
  (if (empty? (editor-pre ed))
      ed
      (make-editor (rest (editor-pre ed))
                   (cons (first (editor-pre ed)) (editor-post ed)))))

; editor-rgt: Editor -> Editor
; move the cursor position one 1String right, if possible

(check-expect (editor-rgt (create-editor "" "")) (create-editor "" ""))
(check-expect (editor-rgt (create-editor "" "hello")) (create-editor "h" "ello"))
(check-expect (editor-rgt (create-editor "hel" "lo")) (create-editor "hell" "o"))
(check-expect (editor-rgt (create-editor "hello" "")) (create-editor "hello" ""))

(define (editor-rgt ed)
  (if
   (empty? (editor-post ed))
   ed
   (make-editor (cons (first (editor-post ed)) (editor-pre ed))
                (rest (editor-post ed)))))

; editor-del: Editor -> Editor
; delete one 1String to the left of the cursor, if possible

(check-expect (editor-del (create-editor "hello" "")) (create-editor "hell" ""))
(check-expect (editor-del (create-editor "hel" "lo")) (create-editor "he" "lo"))
(check-expect (editor-del (create-editor "" "hello")) (create-editor "" "hello"))
(check-expect (editor-del (create-editor "" "")) (create-editor "" ""))

(define (editor-del ed)
  (if 
   (empty? (editor-pre ed))
   ed
   (make-editor (rest (editor-pre ed)) (editor-post ed))))

; create-editor: String String -> Editor
; Create an editor from pre and post strings

(check-expect (create-editor "" "")
              (make-editor empty empty))
(check-expect (create-editor "all" "good")
              (make-editor lla good))

(define (create-editor pre post)
  (make-editor (reverse (explode pre)) (explode post)))

; main : String -> Editor
; launch the editor given some initial string
(define (main s)
  (big-bang (create-editor s "")
            (on-key editor-kh)
            (to-draw editor-render)))
